module Run ( runEnv
           , Opts (..)
           , Result (..)
           , SatDict
           , Log
           , runEnvFirst
           ) where

import qualified Data.Map.Strict as M
import Control.Monad.RWS.Strict
import Control.Monad.State.Strict    as St
import qualified Data.SBV.Internals  as I
import qualified Data.SBV            as S
import qualified Data.SBV.Control    as SC

import Control.Arrow (first, second)

import GHC.Generics
import Control.DeepSeq               (NFData)

import Data.Maybe                    (catMaybes)

import VProp.Types
import VProp.SBV
import VProp.Core
import V

-- | The satisfiable dictionary, this is actually the "state" keys are configs
-- and values are whether that config is satisfiable or not (a bool)
type SatDict a = (M.Map Config Bool, M.Map a Bool) -- keys may incur perf penalty

-- | The optimizations that could be set
data Opts a = Opts { runBaselines :: Bool              -- ^ Run baselines?
                   , runAD :: Bool                     -- ^ run anddecomp baseline? else Brute Force
                   , runOpts :: Bool                   -- ^ Run optimizations or not?
                   , optimizations :: [VProp a -> VProp a] -- ^ a list of optimizations
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
runEnv !base !bAD !bOpt !opts !x = _runEnv
                                   (work x)
                                   (_setOpts base bAD bOpt opts)
                                   (initSt x)

runEnvFirst :: Bool -> Bool -> Bool -> [VProp String -> VProp String] -> VProp String -> IO (V Dim (Maybe I.SMTModel))
runEnvFirst base bAD bOpt opts x = (head . unbox . fst') <$> _runEnv (work x) (_setOpts base bAD bOpt opts) (initSt x)
  where fst' (y,_,_)  = y
        unbox (Vr xs) = xs

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
      plainProps = (\y -> sequence $! (y, selectVariant y prop)) <$> confs
  plainModels <- lift $ mapM (S.sat . symbolicPropExpr . snd) $! catMaybes plainProps
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
            deriving (Generic, Show)

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
    (result,_) <- lift . S.runSMT . incrementalSolve $ St.evalStateT (propToSBool prop) M.empty
    return $ Vr result

type UsedVars a = M.Map a S.SBool
type IncPack a b = St.StateT (UsedVars a) S.Symbolic b

type UsedDims a = M.Map a Bool
type IncState = ([V Dim (Maybe I.SMTModel)], UsedDims Dim)
type IncSolve a = St.StateT IncState SC.Query a

incrementalSolve :: S.Symbolic (VProp S.SBool) -> S.Symbolic IncState
incrementalSolve prop = do prop' <- prop
                           SC.query $ St.execStateT (incrementalSolve_ prop') ([], M.empty)

propToSBool :: VProp String -> IncPack String (VProp S.SBool)
propToSBool = traverse smtBool

smtBool :: String -> IncPack String S.SBool
smtBool str = do st <- get
                 case str `M.lookup` st of
                   Nothing -> do b <- lift $ S.sBool str
                                 St.modify (M.insert str b)
                                 return b
                   Just x  -> return x


bToSb :: S.Boolean p => Bool -> p
bToSb True = S.true
bToSb False = S.false

-- | get a model out given an S.SBool
getModel :: SC.Query (V Dim (Maybe I.SMTModel))
getModel = do cs <- SC.checkSat
              case cs of
                SC.Unk   -> error "Unknown!"
                SC.Unsat -> return (Plain Nothing)
                SC.Sat   -> (Plain . Just) <$> SC.getModel


instance (Monad m, I.SolverContext m) =>
  I.SolverContext (StateT IncState m) where
  constrain = lift . S.constrain
  namedConstraint = (lift .) . S.namedConstraint
  setOption = lift . S.setOption

incHelper :: S.SBool -> [VProp S.SBool] ->
  (S.SBool -> S.SBool -> S.SBool) -> IncSolve S.SBool
incHelper acc ![]     _ = {-# SCC "incHelper" #-} return acc
incHelper acc !(x:xs) f = {-# SCC "incHelper" #-} do b <- incrementalSolve_ x; incHelper (b `f` acc) xs f

incrementalSolve_ :: VProp S.SBool -> IncSolve S.SBool
incrementalSolve_ (Ref b) = return b
incrementalSolve_ (Lit b) = return (bToSb b)
incrementalSolve_ (Not bs)= do b <- incrementalSolve_ (S.bnot <$> bs)
                               S.constrain b
                               return b
incrementalSolve_ (Op2 Impl l r) = do bl <- incrementalSolve_ l
                                      br <- incrementalSolve_ r
                                      return $ (bl S.==> br)
incrementalSolve_ (Op2 BiImpl l r) = do bl <- incrementalSolve_ l
                                        br <- incrementalSolve_ r
                                        S.constrain $ bl S.<=> br
                                        return $ bl S.<=> br
incrementalSolve_ (Opn And ps) = do b <- incHelper S.true ps (S.&&&)
                                    S.constrain b
                                    return b
incrementalSolve_ (Opn Or ps) = do b <- incHelper S.true ps (S.|||)
                                   S.constrain b
                                   return b
incrementalSolve_ (Chc d l r) = {-# SCC "Choice_Solve"#-}
  do (_, used) <- get
     case M.lookup d used of
       Just True  -> incrementalSolve_ l
       Just False -> incrementalSolve_ r
       Nothing    -> do St.modify . second $ M.insert d True
                        lift $ SC.push 1
                        _ <- incrementalSolve_ l
                        lmodel <- lift $ getModel
                        lift $ SC.pop 1

                        St.modify . second $ M.adjust (const False) d
                        lift $ SC.push 1
                        b <- incrementalSolve_ r
                        rmodel <- lift $ getModel
                        lift $ SC.pop 1

                        St.modify . first $ ((:) (VChc d lmodel rmodel))

                        St.modify . second $ M.delete d
                        return b
