module Run ( runEnv
           , evalEnv
           , Opts (..)
           ) where

import Data.Bifunctor (bimap)
import Data.Bifoldable
import Data.Foldable (foldr')
import Data.Maybe (fromJust, isJust)
import qualified Data.Map.Strict as M
import qualified Data.IntMap.Strict as I
import qualified Data.Set as S (fromList)
import Control.Monad.RWS.Strict
import Control.Monad (when)

import VProp
import CNF
import SubProcess

-- | A variable dictionary mapping with dimensions or integers to integers,
-- required for the sat solver
type VarDict d a = M.Map (Either d a) Integer

-- | a intmap that maps integers back to the dimension or value names
type VarDictR d a = I.IntMap (Either d a)

-- | The satisfiable dictionary, this is actually the "state" keys are configs
-- and values are whether that config is satisfiable or not (a bool)
type SatDict = M.Map (Config Integer) Satisfiable -- keys may incur perf penalty

-- | The optimizations that could be set
data Opts d a = Opts { baseline :: Bool  -- ^ True for andDecomp, False for brute
                     , others :: [VProp d a -> VProp d a] -- ^ a list of optimizations
                     }
type Log = String

-- | Takes a dimension d, a value a, and a result r
type St d a = (VarDict d a, SatDict, VarDictR d a) -- ^ convenience type for State
type Env d a r = RWST (Opts d r) Log (St d a) IO r -- ^ the monad stack

-- | An empty reader monad environment, in the future read these from config file
_emptyOpts :: Opts d a
_emptyOpts = Opts { baseline = True -- set to use andDecomp
                 , others = []
                 }

-- | Given a bool and a list of opts return a reader configuration
_setOpts :: Bool -> [VProp d a -> VProp d a] -> Opts d a
_setOpts x ys = Opts { baseline = x
                    , others = ys
                    }

-- | Run the RWS monad with defaults of empty state, reader
runEnv :: Env d a r -> Opts d r -> IO (r, (St d a),  Log)
runEnv m opts = runRWST m opts emptySt

-- | Run the RWS monad and grab the result value
evalEnv :: Env d a r -> Opts d r -> IO r
evalEnv m o = runEnv m o >>= return . f
  where f (x,_,_) = x

-- | An Empty env state is a dictionary of variable names and their hashes and
-- a dictionary for each hash that holds the results of the sat solver
emptySt :: (St d a)
emptySt = (M.empty, M.empty, I.empty)

-- | Given a variational term pack an initial state in the environment Monad
recordVars :: (Ord a
              , Ord d
              , MonadState (St d a) m) => VProp d a -> m (VProp Integer Integer)
recordVars cs = do
  (_, _, _) <- get
  let t = bifoldr'
          (\dim acc -> (Left dim) : acc)
          (\val acc -> (Right val) : acc) [] cs
      pairs = zip t [1..]
      vDict = M.fromList $ zip t [1..]
      vRDict = foldr' (\(dim, int) dict -> I.insert int dim dict) I.empty pairs
      cs' = bimap (\x-> vDict M.! (Left x)) (\x -> vDict M.! (Right x)) cs
      satDict = M.fromList $ zip (paths cs') (repeat False)
  put (vDict, satDict, vRDict)
  return cs'

-- | convert  propositional term to a DIMACS CNF term
propToCNF :: (Num a, Integral a) => String -> GProp a -> CNF
propToCNF str ps = cnf
  where
    cnf = CNF { comment = str
              , vars    = S.fromList $ foldr' ((:) . toInteger) [] ps
              , clauses = orSplit . toListAndSplit $ toInteger <$> ps
              }

-- | main workhorse for running the SAT solver
work :: VProp Integer Integer -> Env Integer Integer (Maybe (VProp Integer Satisfiable))
work cs = do
  bs <- asks baseline
  if bs
    then do (_, sats, rvars) <- get
            let cnf = propToCNF (show cs) . groundGProp . andDecomp $ cs
            res <- lift $ runPMinisat cnf
            return $ recompile (M.toList (M.map (const res) sats))

    else do
            (_, sats, _) <- get
            let keys = M.keys sats
                cnfs = (\y -> (y, select y cs)) <$> keys
            mapM_ work' cnfs
            (_, newSats, rvars) <- get
            return $ (bimap (extract . (rvars I.!) . fromIntegral) id) <$> recompile (M.toList newSats)
  where
    extract (Left a) = a
    extract (Right a) = a

-- | Given a configuration, a boolean representing satisfiability and a Prop, If
-- the prop does not contain a Nothing (as denoted by the bool) then extract the
-- values from the prop, ground then prop, convert to a CNF with descriptor of
-- the configuration, run the sat solver and save the result to the SAT table
-- TODO: Fix this nonsensical type signature
work' :: (Ord d, Show d, Integral a, MonadTrans t2,
           MonadState (t, M.Map (M.Map d Bool) Satisfiable, t1) (t2 IO)) =>
         (M.Map d Bool, Maybe (VProp d a)) -> t2 IO ()
work' (conf, prop) = when (isJust prop) $
  do (vars, sats, rvars) <- get
     result <- lift . runPMinisat . propToCNF (show conf) . fmap fromJust . ground conf . fromJust $ prop
     put (vars, M.insert conf result sats, rvars)


_ex :: VProp String Integer
_ex = Chc "a" (Chc "b" (Ref 1) (Ref 2)) (Chc "c" (Ref 1) (Ref 2))
