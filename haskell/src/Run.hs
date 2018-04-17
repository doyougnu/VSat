module Run ( runEnv
           , Opts (..)
           , Result
           , SatDict
           , Log
           , test2
           ) where

import qualified Data.Map.Strict as M
import Control.Monad.RWS.Strict
import qualified Data.SBV.Internals  as I
import qualified Data.SBV            as S
import qualified Data.SBV.Control    as SC

import qualified Data.Set            as Set

import GHC.Generics
import Control.DeepSeq               (NFData)

import Data.Maybe                    (fromMaybe, catMaybes)

import VProp
import V

-- | The satisfiable dictionary, this is actually the "state" keys are configs
-- and values are whether that config is satisfiable or not (a bool)
type SatDict a = (M.Map Config Bool, M.Map a Bool) -- keys may incur perf penalty

-- | The optimizations that could be set
data Opts a = Opts { runBaselines :: Bool              -- ^ Run baselines?
                 , runAD :: Bool                     -- ^ run anddecomp baseline? else Brute Force
                 , runOpts :: Bool                   -- ^ Run optimizations or not?
                 , optimizations :: [(VProp a) -> (VProp a)] -- ^ a list of optimizations
                 }

-- | Type convenience for Log
type Log = String

-- | Takes a dimension d, a value a, and a result r
type Env a r = RWST (Opts a) Log (SatDict a) IO r -- ^ the monad stack

-- | An empty reader monad environment, in the future read these from config file
_emptyOpts :: (Opts a)
_emptyOpts = Opts { runBaselines = False
                  , runAD = False
                  , runOpts = False
                  , optimizations = []
                  }


_setOpts :: Bool -> Bool -> Bool -> [VProp a -> VProp a] -> Opts a
_setOpts base bAD bOpt opts = Opts { runBaselines = base
                                  , runAD = bAD
                                  , runOpts = bOpt
                                  , optimizations = opts
                                  }



-- | Run the RWS monad with defaults of empty state, reader
_runEnv :: Env a r -> Opts a -> (SatDict a) -> IO (r, (SatDict a),  Log)
_runEnv m opts st = runRWST m opts st

-- TODO use configurate and load the config from a file
runEnv :: Bool -> Bool -> Bool -> [VProp String -> VProp String] -> VProp String -> IO (Result, (SatDict String), Log)
runEnv base bAD bOpt opts x = _runEnv
                             (work x)
                             (_setOpts base bAD bOpt opts)
                             (initSt x)


-- | Given a VProp a term generate the satisfiability map
initSt :: (Show a, Ord a) => VProp a -> (SatDict a)
initSt prop = (sats, vs)
  where sats = M.fromList . fmap (\x -> (x, False)) $ M.fromList <$> configs prop
        vs = M.fromSet (const False) (vars prop)


-- | Some logging functions
_logBaseline :: (Show a, MonadWriter [Char] m) => a -> m ()
_logBaseline x = tell $ "Running baseline: " ++ show x

_logCNF :: (Show a, MonadWriter [Char] m) => a -> m ()
_logCNF x = tell $ "Generated CNF: " ++ show x

_logResult :: (Show a, MonadWriter [Char] m) => a -> m ()
_logResult x = tell $ "Got result: " ++ show x


-- | Run the brute force baseline case, that is select every plain variant and
-- run them to the sat solver
runBruteForce :: (Show a, Ord a) =>
  (MonadTrans t, MonadState (SatDict a) (t IO)) => VProp a -> t IO [S.SatResult]
runBruteForce prop = {-# SCC "brute_force"#-} do
  (_confs, _) <- get
  let confs = M.keys _confs
      plainProps = (\y -> sequence $ (y, selectVariant y prop)) <$> confs
  -- this line is always throwing a Nothing
  plainModels <- lift $ mapM (S.sat . symbolicPropExpr . snd) $ catMaybes plainProps
  return plainModels

-- | Run the and decomposition baseline case, that is deconstruct every choice
-- and then run the sat solver
runAndDecomp :: VProp String -> IO (Maybe I.SMTModel)
runAndDecomp prop = {-# SCC "andDecomp" #-} S.runSMT $ do
  p <- symbolicPropExpr $ (andDecomp prop dimName)
  S.constrain p
  SC.query $ do
    c <- SC.checkSat
    case c of
      SC.Unk -> error "asdf"
      SC.Unsat -> return Nothing
      SC.Sat -> do model' <- SC.getModel
                   return $ Just model'

-- | main workhorse for running the SAT solver
data Result = R (Maybe I.SMTModel)
            | L [S.SatResult]
            | Vr [V Dim (Maybe I.SMTModel)]
            deriving Generic

instance NFData Result

work :: ( MonadTrans t
        , MonadState (SatDict String) (t IO)
        , MonadReader (Opts String) (t IO)) => VProp String -> t IO Result
work prop = do
  baselines <- asks runBaselines
  bAD <- asks runAD
  -- fix this antipattern later
  if baselines
    then if bAD
         then lift $ runAndDecomp prop  >>= return . R
         else do
    runBruteForce prop >>= return . L
    else do
    opts <- asks optimizations
    result <- lift $ incrementalSolve prop opts
    return $ Vr result

-- | given VProp a, incrementally solve it using variational tricks and SBV
incrementalSolve :: (Show a, Ord a) => VProp a -> [VProp a -> VProp a] -> IO [V Dim (Maybe I.SMTModel)]
incrementalSolve prop opts = {-# SCC "choice_solver"#-} solveChoice prop

-- | given a prop, check if it is plain, if so run SMT normally, if not run the
-- | variational solver
solveChoice :: (Show a, Ord a) => VProp a -> IO [V Dim (Maybe I.SMTModel)]
solveChoice prop
  | isPlain prop = fmap pure $ S.runSMT $ do
      p <- symbolicPropExpr prop
      S.constrain p
      SC.query $ do c <- SC.checkSat
                    case c of
                      SC.Unk   -> error "SBV failed in plain solve Choice"
                      SC.Unsat -> return $ Plain Nothing
                      SC.Sat   -> SC.getModel >>= return . Plain . Just
  | otherwise = S.runSMT . test2 $ show <$> prop

test2 :: VProp String -> S.Symbolic [V Dim (Maybe I.SMTModel)]
test2 prop = do
  -- prop' <- traverse (\p -> sequence (p, S.sBool p)) prop -- phase 1, add all vars
  prop' <- traverse S.sBool prop
  (_, models) <- SC.query $ loop prop' []
  return models

bToSb True = S.true
bToSb False = S.false

-- | get a model out given an S.SBool
getModel :: SC.Query (V Dim (Maybe I.SMTModel))
getModel = do cs <- SC.checkSat
              case cs of
                SC.Unk   -> error "Unknown!"
                SC.Unsat -> return (Plain Nothing)
                SC.Sat   -> (Plain . Just) <$> SC.getModel

constrainCheck :: String -> Set.Set String -> Bool
constrainCheck = Set.member

smartConstrain :: (String, S.SBool) -> Set.Set String -> SC.Query ()
smartConstrain (s, sb) vs
  | constrainCheck s vs = return ()
  | otherwise = S.constrain sb

loop :: VProp S.SBool -> [V Dim (Maybe I.SMTModel)]
  -> SC.Query (S.SBool, [V Dim (Maybe I.SMTModel)])
loop (Ref b) acc = do S.constrain b; return (b, acc)
loop (Lit b) acc = do S.constrain (bToSb b); return (bToSb b, acc)
loop (Not bs) acc = do (b, acc') <- loop (S.bnot <$> bs) acc
                       S.constrain b
                       return (b, acc' ++ acc)
loop (Op2 Impl l r) acc = do (bl, al) <- loop l acc
                             (br, ar) <- loop r al
                             S.constrain $ bl S.==> br
                             return (bl S.==> br, ar)
loop (Op2 BiImpl l r) acc = do (bl, al) <- loop l acc
                               (br, ar) <- loop r al
                               S.constrain $ bl S.<=> br
                               return (bl S.<=> br, ar)
loop (Opn And ps) acc = do (bs, as) <- go ps S.true acc
                           S.constrain bs
                           return (bs, as)
  where
    go [] bacc ac = return (bacc, ac)
    go (s:ss) bacc ac = do (sB, ac') <- loop s ac
                           go ss (sB S.&&& bacc) ac'
loop (Opn Or ps) acc = do (bs, as) <- go ps S.true acc
                          S.constrain bs
                          return (bs, as)
  where
    go [] bacc ac = return (bacc, ac)
    go (s:ss) bacc ac = do (sB, ac') <- loop s ac
                           go ss (sB S.||| bacc) ac'
loop (Chc d l r) acc =
  do SC.push 1
     (_, acc') <- loop l acc
     lmodel <- getModel
     SC.pop 1
     SC.push 1
     (a, racc) <- loop r (acc' ++ acc)
     rmodel <- getModel
     SC.pop 1
     return $ (a, (VChc d lmodel rmodel) : racc)
