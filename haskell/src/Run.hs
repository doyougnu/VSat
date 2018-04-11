module Run ( runEnv
           , Opts (..)
           , Result
           , SatDict
           , Log
           ) where

import qualified Data.Map.Strict as M
import Control.Monad.RWS.Strict
import qualified Data.SBV.Internals  as I
import qualified Data.SBV            as S
import qualified Data.SBV.Control    as SC

import qualified Data.Set            as Set
import Data.List                     (partition, (\\), nub, lookup)
import Data.Char                     (isUpper)

import GHC.Generics
import Control.DeepSeq               (NFData)

import Data.Maybe                    (fromJust, fromMaybe, catMaybes)

import VProp
import V

-- | The satisfiable dictionary, this is actually the "state" keys are configs
-- and values are whether that config is satisfiable or not (a bool)
type SatDict = (M.Map Config Bool, M.Map Var Bool) -- keys may incur perf penalty

-- | The optimizations that could be set
data Opts = Opts { runBaselines :: Bool              -- ^ Run baselines?
                 , runAD :: Bool                     -- ^ run anddecomp baseline? else Brute Force
                 , runOpts :: Bool                   -- ^ Run optimizations or not?
                 , optimizations :: [VProp -> VProp] -- ^ a list of optimizations
                 }

-- | Type convenience for Log
type Log = String

-- | Takes a dimension d, a value a, and a result r
type Env r = RWST Opts Log SatDict IO r -- ^ the monad stack

-- | An empty reader monad environment, in the future read these from config file
_emptyOpts :: Opts
_emptyOpts = Opts { runBaselines = False
                  , runAD = False
                  , runOpts = False
                  , optimizations = []
                  }


_setOpts :: Bool -> Bool -> Bool -> [VProp -> VProp] -> Opts
_setOpts base bAD bOpt opts = Opts { runBaselines = base
                                  , runAD = bAD
                                  , runOpts = bOpt
                                  , optimizations = opts
                                  }



-- | Run the RWS monad with defaults of empty state, reader
_runEnv :: Env r -> Opts -> SatDict -> IO (r, SatDict,  Log)
_runEnv m opts st = runRWST m opts st

-- TODO use configurate and load the config from a file
runEnv :: Bool -> Bool -> Bool -> [VProp -> VProp] -> VProp -> IO (Result, SatDict, Log)
runEnv base bAD bOpt opts x = _runEnv
                             (work x)
                             (_setOpts base bAD bOpt opts)
                             (initSt x)


-- | Given a VProp term generate the satisfiability map
initSt :: VProp -> SatDict
initSt prop = (sats, vs)
  where sats = M.fromList . fmap (\x -> (x, False)) $ M.fromList <$> choices prop
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
runBruteForce :: (MonadTrans t, MonadState SatDict (t IO)) => VProp -> t IO [S.SatResult]
runBruteForce prop = {-# SCC "brute_force"#-} do
  (_confs, _) <- get
  let confs = M.keys _confs
      plainProps = (\y -> sequence $ (y, selectVariant y prop)) <$> confs
  -- this line is always throwing a Nothing
  plainModels <- lift $ mapM (S.sat . symbolicPropExpr . snd) $ catMaybes plainProps
  return plainModels

-- | Run the and decomposition baseline case, that is deconstruct every choice
-- and then run the sat solver
runAndDecomp :: VProp -> IO (Maybe I.SMTModel)
runAndDecomp prop = {-# SCC "andDecomp" #-} S.runSMT $ do
  p <- symbolicPropExpr $ (andDecomp prop)
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
        , MonadState SatDict (t IO)
        , MonadReader Opts (t IO)) => VProp -> t IO Result
work prop = do
  baselines <- asks runBaselines
  bAD <- asks runAD
  -- fix this antipattern later
  if baselines
    then if bAD
         then lift $ runAndDecomp prop >>= return . R
         else do
    runBruteForce prop >>= return . L
    else do
    opts <- asks optimizations
    result <- lift $ incrementalSolve prop opts
    return $ Vr result

-- | given VProp, incrementally solve it using variational tricks and SBV
incrementalSolve :: VProp -> [VProp -> VProp] -> IO [V Dim (Maybe I.SMTModel)]
incrementalSolve prop opts = {-# SCC "choice_solver"#-} solveChoice prop

-- | convert a list of dims to symbolic dims, and keep the association
dimBoolMap :: [Dim] -> S.Symbolic [(Dim, S.SBool)]
dimBoolMap =  traverse (\x -> sequence (x, S.sBool $ dimName x))

-- | combine a list of dims and symbolic dims with a list of configs, this returns
-- a config with the symbolic dim in the snd position
mkPaths :: [(Dim, S.SBool)] -> [[(Dim, Bool)]] -> [[(Dim, S.SBool, Bool)]]
mkPaths dimBools pths = fmap (\(dim, bl) ->
                                (dim, fromJust $ lookup dim dimBools, bl)) <$> pths

-- | Given a 3 tuple use the symbolic dim and the boolean to set a query constraint
cConstrain :: (Dim, S.SBool) -> Bool -> SC.Query ()
cConstrain (_, sDim) bl = assocToConstraint (sDim, bl) >>= S.constrain

-- | perform a query with a choice expression. This assumes the query monad
-- knows about the dimension variables, the plain variables, and the expression
-- to solve. It then pushes teh assertion stack, constrains the dimension
-- variables according to its config, then pops the assertion stack and returns
-- the internal model
_cQuery :: (Dim, S.SBool) -> Bool -> SC.Query (Maybe I.SMTModel)
_cQuery x bl = do SC.push 1
                  cConstrain x bl
                  c <- SC.checkSat
                  case c of
                    SC.Unk   -> error "asdf"
                    SC.Unsat -> return Nothing
                    SC.Sat   -> do model' <- SC.getModel
                                   SC.pop 1
                                   return $ Just model'

cQuery :: (Dim, S.SBool) -> SC.Query (V Dim (Maybe I.SMTModel))
cQuery x@(dim, _) = do
  trueModel <- Plain <$> _cQuery x True
  falseModel <- Plain <$> _cQuery x False
  return $ VChc dim trueModel falseModel

-- | given a prop, check if it is plain, if so run SMT normally, if not run the
-- | variational solver
solveChoice :: VProp -> IO [V Dim (Maybe I.SMTModel)]
solveChoice prop
  | isPlain prop = fmap pure $ S.runSMT $ do
      p <- symbolicPropExpr prop
      S.constrain p
      SC.query $ do c <- SC.checkSat
                    case c of
                      SC.Unk   -> error "SBV failed in plain solve Choice"
                      SC.Unsat -> return $ Plain Nothing
                      SC.Sat   -> SC.getModel >>= return . Plain . Just
  | otherwise = S.runSMT $ do
      (p, ds') <- symbolicPropExpr' prop
      S.constrain p
      let ds = M.toList ds'
      res <- SC.query $ do mapM (cQuery) ds
      return res

-- | Given a list of free variables, add them all together with SBV guessed
-- integers
ericTest :: [String] -> S.Symbolic (Maybe Integer)
ericTest [] = return . return $ 0
ericTest (x:xs) = do
  a <- S.sInteger x
  a' <- SC.query $ go a
  b' <- ericTest xs -- this line will throw an exception for nested querys
  return $ liftM2 (+) a' b'
  where
    -- | take a symbolic integer, get the value, print it, and return it
    go :: S.SInteger -> SC.Query (Maybe Integer)
    go x' = do cs <- SC.checkSat
               case cs of
                 SC.Unk   -> error "Solver said unknown!"
                 SC.Unsat -> return Nothing -- no solution!
                 SC.Sat   -> do xValue <- SC.getValue x'
                                SC.io . print $ "I got a " ++ show xValue
                                return $ Just xValue


-- | Given two models, if both are not nothing, combine them
combineModels :: Maybe I.SMTModel -> Maybe I.SMTModel -> Maybe I.SMTModel
combineModels Nothing a = a
combineModels a Nothing = a
combineModels
  (Just I.SMTModel{I.modelAssocs=aAs, I.modelObjectives=aOs})
  (Just I.SMTModel{I.modelAssocs=bAs , I.modelObjectives=bOs}) =
  (Just I.SMTModel{ I.modelAssocs= nub aAs ++ bAs
                  , I.modelObjectives = aOs ++ bOs})

-- | Given an association between a symbolic bool variable and a normal bool,
-- add a representative constraint to the query monad
assocToConstraint :: (S.SBool, Bool) -> SC.Query S.SBool
assocToConstraint (var, val) = return $ var S..== (bToSb val)
  where bToSb True = S.true
        bToSb False = S.false

-- | Change a prop to a predicate, avoiding anything that has already been assigned
symbolicPropExpr' :: VProp -> S.Symbolic (S.SBool, M.Map Dim S.SBool)
symbolicPropExpr' prop = do
    let vs = (Set.toList (vars prop))
        ds = (Set.toList (dimensions prop))
    syms <- fmap (M.fromList . zip vs) (S.sBools (map varName vs))
    dims <- fmap (M.fromList . zip ds) (S.sBools (map dimName ds))
    let look f = fromMaybe err (M.lookup f syms)
        lookd d = fromMaybe errd (M.lookup d dims)
    return ((evalPropExpr lookd look prop), dims)

  where err = error "symbolicPropExpr: Internal error, no symbol found."
        errd = error "symbolicPropExpr: Internal error, no dimension found."
