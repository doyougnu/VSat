module Run ( runEnv
           , evalEnv
           , Opts (..)
           ) where

import Data.Bifunctor (bimap)
import Data.List (nub)
import Data.Bifoldable
import Data.Foldable (foldr')
-- import Data.Maybe (fromJust, isJust)
import qualified Data.Map.Strict as M
-- import qualified Data.IntMap.Strict as I
import qualified Data.Set as S (fromList)
import Control.Monad.RWS.Strict
-- import Control.Monad (when)

import VProp
import qualified CNF as C
import SubProcess

-- | A variable dictionary mapping with dimensions or integers to integers,
-- required for the sat solver
type VarDict d a = M.Map (Either d a) Int

-- | a intmap that maps integers back to the dimension or value names
type VarDictR d a = M.Map Int (Either d a)

-- | The satisfiable dictionary, this is actually the "state" keys are configs
-- and values are whether that config is satisfiable or not (a bool)
type SatDict = M.Map (Config Int) Satisfiable -- keys may incur perf penalty

-- | The optimizations that could be set
data Opts d a = Opts { baseline :: Bool  -- ^ True for andDecomp, False for brute
                     , optimizations :: [VProp d a -> VProp d a] -- ^ a list of optimizations
                     , vars :: VarDict d a
                     , rvars :: VarDictR d a
                     }

-- | Type convenience for Log
type Log = String

-- | Type convenience for State
type St d a = SatDict

-- | Takes a dimension d, a value a, and a result r
type Env d a r = RWST (Opts d a) Log (St d a) IO r -- ^ the monad stack

-- | An empty reader monad environment, in the future read these from config file
_emptyOpts :: Opts d a
_emptyOpts = Opts { baseline = True -- set to use andDecomp
                 , optimizations = []
                 , vars = M.empty
                 , rvars = M.empty
                 }

-- | Run the RWS monad with defaults of empty state, reader
runEnv :: Env d a r -> Opts d a -> IO (r, St d a,  Log)
runEnv m opts = runRWST m opts emptySt

-- | Run the RWS monad and grab the result value
evalEnv :: Env d a r -> Opts d a -> IO r
evalEnv m o = f <$> runEnv m o
  where f (x,_,_) = x

-- | An Empty env state is a dictionary of variable names and their hashes and
-- a dictionary for each hash that holds the results of the sat solver
emptySt :: (St d a)
emptySt = M.empty

-- | Given a vprop collapse it to a list of dimensions and values
collect :: (Eq a, Eq d) => VProp d a -> [Either d a]
collect = nub . bifoldr'
          (\dim acc -> Left dim : acc)
          (\val acc -> Right val : acc) []

-- | Given a list of dimensions and values and an integer construct the vardict
genVDict :: (Ord a, Ord d) => [(Either d a, Int)] -> VarDict d a
genVDict = M.fromList

-- | Given a list of dimensions and values and an integer construct the reverse
-- vardict
genRVDict :: [(Either d a, Int)] -> VarDictR d a
genRVDict = foldr' (\(dim, int) dict -> M.insert int dim dict) M.empty

-- | Given a variable dictionary and a vprop term. Construct a representative
-- VProp with only integers for both dimensions and variables
unify :: (Ord d, Ord a) => VarDict d a -> VProp d a ->  VProp Int Int
unify vDict = bimap ((vDict M.!) . Left) ((vDict M.!) . Right)


-- | Flatten the VProp term to a homogeneous list
flatten :: (Eq a, Eq d) => VProp d a -> [Either d a]
flatten = nub . bifoldr' (\dim acc -> Left dim : acc)
                   (\val acc -> Right val : acc) []


-- | extract an element from a Either term
getL :: Either d a -> d
getL (Left a) = a
getL (Right _) = error "You've called getL on Right!"

getR :: Either d a -> a
getR (Right a) = a
getR (Left _) = error "You've called getR on Left!"

-- | Given a VProp term prepare the runtime environment
_recordVars :: (Ord a , Ord d) => VProp d a -> Opts d a -> Opts d a
_recordVars cs opts = Opts { baseline = baseline opts
                          , optimizations = optimizations opts
                          , vars = genVDict numberedProp
                          , rvars = genRVDict numberedProp
                          }
  where numberedProp = zip (flatten cs) [1..]


-- | convert  propositional term to a DIMACS CNF term
propToCNF :: (Num a, Integral a) => String -> GProp a -> C.CNF
propToCNF str ps = cnf
  where
    cnf = C.CNF { C.comment = str
                , C.vars    = S.fromList $ foldr' ((:) . toInteger) [] ps
                , C.clauses = orSplit . toListAndSplit $ toInteger <$> ps
                }

-- | given a variable dictionary and a vprop, replace all dimenions with the
-- values in the dict
packProp :: (Ord a, Ord d) => VProp d a -> VarDict d a -> VProp Int Int
packProp ps dict = bimap ((dict M.!) . Left) ((dict M.!) . Right) ps


-- | Given a reverse variable dictionary and a VProp Int a replace all
-- dimensions with their values in the reverse variable dictionary
unPackProp :: (Ord a, Ord d) => VProp Int Int -> VarDictR d a -> VProp d a
unPackProp ps dict = bimap (getL . (dict M.!)) (getR . (dict M.!)) ps


-- | main workhorse for running the SAT solver
-- FIXE THE ENGINE CALL SO YOU CAN RUN SOMETHING
-- run like: runEnv (work _ex) (_recordVars _ex _emptyOpts)
work cs = do
  bs <- asks baseline
  vdict <- asks vars
  sats <- get
  let cnf = propToCNF (show cs) . groundGProp . andDecomp $ packProp cs vdict
  lift $ print (packProp cs vdict)
  res <- lift $ runPMinisat cnf
  let aa = recompile . M.toList $ M.map (const res) sats
  if bs
    then do
    -- return $ bimap (\x -> rDict M.! x) id <$> aa
    return aa

    else do
    -- let keys = M.keys sats
    -- cnfs = (\y -> (y, select y cs)) <$> keys
    -- mapM_ work' cnfs
    -- (_, newSats, rvars) <- get
    -- let x = recompile (M.toList newSats)
    -- y = bimap (yankOut rvars) id  <$> y
    -- return $ recompile (M.toList (M.map (const res) sats))
    return aa


-- -- | Given a configuration, a boolean representing satisfiability and a Prop, If
-- -- the prop does not contain a Nothing (as denoted by the bool) then extract the
-- -- values from the prop, ground then prop, convert to a CNF with descriptor of
-- -- the configuration, run the sat solver and save the result to the SAT table
-- -- TODO: Fix this nonsensical type signature
-- work' (conf, prop) = when (isJust prop) $
--   do (vars, sats, rvars) <- get
--      result <- lift . runPMinisat . propToCNF (show conf) . fmap fromJust . ground conf . fromJust $ prop
--      put (vars, M.insert conf result sats, rvars)


_ex :: VProp String Int
_ex = Chc "a" (Chc "b" (Ref 1) (Ref 2)) (Chc "c" (Ref 1) (Ref 2))

_ex2 :: VProp Int Int
_ex2 = Chc 0 (Chc 1 (Ref 1) (Ref 2)) (Chc 2 (Ref 1) (Ref 2))
