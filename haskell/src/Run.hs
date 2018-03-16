module Run ( runEnv
           , Opts (..)
           ) where

import qualified Data.Map.Strict as M
import Control.Monad.RWS.Strict
import Data.SBV.Internals
import Data.SBV
import Data.SBV.Control

import qualified Data.Set            as Set
import Data.Foldable                 (foldr')

import Data.SBV                      (isSatisfiable)
import Data.Maybe                    (fromJust, isJust, fromMaybe)

import VProp

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
runEnv :: Bool -> Bool -> Bool -> [VProp -> VProp] -> VProp -> IO (VProp, SatDict, Log)
runEnv base bAD bOpt opts x = _runEnv
                             (work x)
                             (_setOpts base bAD bOpt opts)
                             (initSt x)


-- | Given a VProp term generate the satisfiability map
initSt :: VProp -> SatDict
initSt prop = (sats, vs)
  where sats = M.fromList . fmap (\x -> (x, False)) . Set.toList $ paths prop
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
runBruteForce :: (MonadTrans t, MonadState SatDict (t IO)) => VProp -> t IO VProp
runBruteForce prop = do
  (_confs, _) <- get
  let confs = M.keys _confs
      plainProps = (\y -> (y, selectVariant y prop)) <$> confs
  mapM_ work' plainProps
  (newSats, _) <- get
  return . recompile prop $ (\(x, y) -> (x, show y)) <$> M.toList newSats

-- | Run the and decomposition baseline case, that is deconstruct every choice
-- and then run the sat solver
runAndDecomp :: (MonadTrans t, MonadState SatDict (t IO)) => VProp -> t IO VProp
runAndDecomp prop = do
  (sats, _) <- get
  result <- lift . isSatisfiable . symbolicPropExpr $ (andDecomp prop)
  return . recompile prop . fmap (\(x, y) -> (x, show y)) . M.toList $ M.map (const result) sats

-- | given a VProp term update the state with the result of a isSatisfiable check
modifySt :: (MonadState SatDict m, Monad m) => VProp -> Bool -> m ()
modifySt vprop b = do
  (confs, vs) <- get
  let variables = vars vprop
  put (confs, Set.foldr' (M.adjust (const b)) vs variables)

-- | Given a VProp update the VProp variables with their satisfiability results
updateProp :: (MonadState SatDict m, Monad m) => VProp -> m VProp
updateProp prop = do
  (_, vs) <- get
  let updatedVars = M.filter id vs
      keys  = M.keys updatedVars
  return $ foldr'
    (\var acc -> alterToLit var (const $ (M.!) updatedVars var) acc)
    prop keys

-- | If then else, the way it should've been defined in Prelude
if' :: Bool -> a -> a -> a
if' True a _  = a
if' False _ b = b

-- | pick a term to evaluate
-- select :: VProp -> VProp
-- select (Opn And (x:_)) = x

-- | main workhorse for running the SAT solver
work :: ( MonadTrans t
        , MonadState SatDict (t IO)
        , MonadReader Opts (t IO)) => VProp -> t IO VProp
work prop = do
  baselines <- asks runBaselines
  bAD <- asks runAD
  -- fix this antipattern later
  if baselines
    then if' bAD (runAndDecomp prop) (runBruteForce prop)
    else if' bAD (runAndDecomp prop) (runBruteForce prop)

-- | Solve a vprop expression by choosing a subterm, solving it, updating the
-- state and repeating
incrementalSolve :: VProp -> Symbolic (Maybe SMTModel)
incrementalSolve prop = do
  Just p <- return . Run.select $ toCNF prop
  res <- incrementalQuery p Nothing
  return res

incrementalQuery :: VProp -> Maybe SMTModel -> Symbolic (Maybe SMTModel)
incrementalQuery prop model = do
  p <- symbolicPropExpr prop
  constrain p
  modelToConstraint model
  query $ do
    c <- checkSat
    case c of
      Unk -> error "asdf"
      Unsat -> return Nothing
      Sat -> do model' <- getModel
                return $ Just model'

assocToConstraint :: (String, CW) -> Symbolic ()
assocToConstraint (var, val) = do v <- sBool var
                                  constrain $ v .== (bToSb boolVal)
                                    where boolVal = cwToBool val
                                          bToSb True = true
                                          bToSb False = false

modelToConstraint :: Maybe SMTModel -> Symbolic ()
modelToConstraint _model =
  do model <- return $ fromMaybe emptyModel _model
     mapM_ assocToConstraint (modelAssocs model)
       where emptyModel = SMTModel {modelAssocs = [], modelObjectives = []}



select :: VProp -> Maybe VProp
select (Opn _ ps) = safeHead [ p | p <- ps ]
  where safeHead [] = Nothing
        safeHead (x:_) = Just x
select (Op2 _ l _) = Just l
select (VProp.Not x) = Just x
select (Chc _ l _) = Just l
select x         = Just x

work' :: ( MonadTrans t
         , MonadState SatDict (t IO)) => (Config, Maybe VProp) -> t IO ()
work' (conf, plainProp) = when (isJust plainProp) $
  do (sats, vs) <- get
     result <- lift . isSatisfiable . symbolicPropExpr . fromJust $ plainProp
     put (M.insert conf result sats, vs)
