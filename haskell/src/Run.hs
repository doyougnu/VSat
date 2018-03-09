module Run ( runEnv
           , Opts (..)
           ) where

import qualified Data.Map.Strict as M
import Control.Monad.RWS.Strict

import qualified Data.Set            as Set

import VProp
import Data.SBV                      (AllSatResult, allSat)

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


runEnv :: Bool -> VProp -> IO (IO AllSatResult, SatDict, Log)
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

-- | main workhorse for running the SAT solver
work :: VProp -> Env (IO AllSatResult)
work = return . allSat . symbolicPropExpr
