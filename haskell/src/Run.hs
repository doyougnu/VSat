module Run ( runEnv
           , Opts (..)
           ) where

import qualified Data.Map.Strict as M
import Control.Monad.RWS.Strict

import qualified Data.Set            as Set

import Data.SBV                      (isSatisfiable)
import Data.Maybe                    (fromJust, isJust)

import VProp

-- | The satisfiable dictionary, this is actually the "state" keys are configs
-- and values are whether that config is satisfiable or not (a bool)
type SatDict = M.Map Config Bool -- keys may incur perf penalty

-- | The optimizations that could be set
data Opts = Opts { baseline :: Bool  -- ^ Run optimizations or not?
                 , optimizations :: [VProp -> VProp] -- ^ a list of optimizations
                 }

-- | Type convenience for Log
type Log = String

-- | Takes a dimension d, a value a, and a result r
type Env r = RWST Opts Log SatDict IO r -- ^ the monad stack

-- | An empty reader monad environment, in the future read these from config file
_emptyOpts :: Opts
_emptyOpts = Opts { baseline = False -- set to use andDecomp
                  , optimizations = []
                  }

_setOpts :: Bool -> Opts
_setOpts b = Opts { baseline = b
                  , optimizations = []
                  }



-- | Run the RWS monad with defaults of empty state, reader
_runEnv :: Env r -> Opts -> SatDict -> IO (r, SatDict,  Log)
_runEnv m opts st = runRWST m opts st


runEnv :: Bool -> VProp -> IO (VProp, SatDict, Log)
runEnv b x = _runEnv (work x) (_setOpts b) (initSt x)


-- | Given a VProp term generate the satisfiability map
initSt :: VProp -> SatDict
initSt vs = M.fromList . fmap (\x -> (x, False)) . Set.toList $ paths vs


-- | Some logging functions
_logBaseline :: (Show a, MonadWriter [Char] m) => a -> m ()
_logBaseline x = tell $ "Running baseline: " ++ show x

_logCNF :: (Show a, MonadWriter [Char] m) => a -> m ()
_logCNF x = tell $ "Generated CNF: " ++ show x

_logResult :: (Show a, MonadWriter [Char] m) => a -> m ()
_logResult x = tell $ "Got result: " ++ show x


runBruteForce :: (MonadTrans t, MonadState SatDict (t IO)) => VProp -> t IO VProp
runBruteForce prop = do
  _confs <- get
  let confs = M.keys _confs
      plainProps = (\y -> (y, selectVariant y prop)) <$> confs
  mapM_ work' plainProps
  newSats <- get
  return . fromJust . recompile $ (\(x, y) -> (x, show y)) <$> M.toList newSats

runAndDecomp :: (MonadTrans t, MonadState SatDict (t IO)) => VProp -> t IO VProp
runAndDecomp prop = do
  sats <- get
  result <- lift . isSatisfiable . symbolicPropExpr $ (andDecomp prop)
  return . fromJust . recompile . fmap (\(x, y) -> (x, show y)) . M.toList $ M.map (const result) sats


-- | main workhorse for running the SAT solver
work :: ( MonadTrans t
        , MonadState SatDict (t IO)
        , MonadReader Opts (t IO)) => VProp -> t IO VProp
work prop = do
  b <- asks baseline
  if b then runAndDecomp prop else runBruteForce prop


work' :: ( MonadTrans t
         , MonadState SatDict (t IO)) => (Config, Maybe VProp) -> t IO ()
work' (conf, plainProp) = when (isJust plainProp) $
  do sats <- get
     result <- lift . isSatisfiable . symbolicPropExpr . fromJust $ plainProp
     put (M.insert conf result sats)
