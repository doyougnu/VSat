module Run ( runEnv
           , initAndRun
           , evalEnv
           , Opts (..)
           ) where

import Data.Hashable as H
import Data.Bifunctor (bimap)
import Data.Bifoldable
import Data.Foldable (foldr')
import Data.Maybe (fromJust, isJust)
import qualified Data.IntMap.Strict as I
import qualified Data.Map.Strict as M
import qualified Data.Set as S (fromList)
import Control.Monad.RWS.Strict
import Control.Monad (when)

import VProp
import CNF
import SubProcess

-- hold an Int to apply labels, hold d set of chars to track which dimension
-- have been seen already
type VarDict d = I.IntMap d
type SatDict d = M.Map (Config d) Satisfiable -- keys may incur perf penalty
data Opts d a = Opts { baseline :: Bool -- ^ True for andDecomp, False for brute
                   , others :: [VProp d a -> VProp d a] -- ^ a list of optimizations
                   }
type Log = String

-- | Global state TODO: Use ReaderT pattern instead of state monad
-- Takes a dimension d, a value a, and a result r
type St d = (VarDict d, SatDict d)
type Env d r = RWST (Opts d r) Log (St d) IO r

-- | An empty reader monad environment, in the future read these from config file
emptyOpts :: Opts d a
emptyOpts = Opts { baseline = True -- set to use andDecomp
                 , others = []
                 }

-- | Run the RWS monad with defaults of empty state, reader
runEnv :: Env d r -> Opts d r -> IO (r, (VarDict d, SatDict d),  Log)
runEnv m opts = runRWST m opts emptySt

-- | Run the RWS monad and grab the result value
evalEnv :: Env d r -> Opts d r -> IO r
evalEnv m o = runEnv m o >>= return . f
  where f (x,_,_) = x

-- | An Empty env state is a dictionary of variable names and their hashes and
-- a dictionary for each hash that holds the results of the sat solver
emptySt :: (VarDict d, SatDict d)
emptySt = (I.empty, M.empty)

-- | Given a variational term pack an initial state in the environment Monad
recordVars :: (Eq d
              , Ord d
              , H.Hashable d
              , MonadState (VarDict d, SatDict d) m) => VProp d a -> m ()
recordVars cs = do
  st@(_, old_sats) <- get
  let (newvars, _)=
        bifoldr'
        (\dim (vars, sats) -> (I.insert (abs . hash $ dim) dim vars , sats))
        (\_ s -> s) st cs
      ss' = M.union old_sats . M.fromList $ zip (paths cs) (repeat False)
  put (newvars, ss')

-- | Unify the dimension and value in d choice to the same type using bifunctor
-- add all dimensions and their hashes to the variable dictionary
unify :: (Integral a, H.Hashable d) => VProp d a -> VProp Integer Integer
unify = bimap (toInteger . abs . hash) toInteger

-- | orient the state monad to run the sat solver
toPropDecomp :: (H.Hashable d, Integral a) => VProp d a -> VProp Integer Integer
toPropDecomp = andDecomp . unify

-- | convert  propositional term to a DIMACS CNF term
propToCNF :: (Num a, Integral a) => String -> GProp a -> CNF
propToCNF str ps = cnf
  where
    cnf = CNF { comment = str
              , vars    = S.fromList $ foldr' ((:) . toInteger) [] ps
              , clauses = orSplit . toListAndSplit $ toInteger <$> ps
              }

-- | main workhorse for running the SAT solver
work :: (Eq d
        , Show a
        , Show d
        , Ord d
        , Hashable d
        , Integral a) => VProp d a -> Env d (Maybe (VProp d Satisfiable))
work cs = do
  bs <- asks baseline
  if bs
    then do (_, sats) <- get
            let cnf = propToCNF (show cs) . groundGProp . toPropDecomp $ cs
            res <- lift $ runPMinisat cnf
            return $ recompile (M.toList (M.map (const res) sats))

    else do
            (_, sats) <- get
            let keys = M.keys sats
                cnfs = (\y -> (y, select y cs)) <$> keys
            mapM_ work' cnfs
            (_, newSats) <- get
            return $ recompile (M.toList newSats)

initAndRun :: (Eq d
              , Show a
              , Show d
              , Ord d
              , H.Hashable d
              , Integral a) =>
              VProp d a -> Env d (Maybe (VProp d Satisfiable))
initAndRun cs = do
  recordVars cs -- initialize the environment
  work cs

-- | Given a configuration, a boolean representing satisfiability and a Prop, If
-- the prop does not contain a Nothing (as denoted by the bool) then extract the
-- values from the prop, ground then prop, convert to a CNF with descriptor of
-- the configuration, run the sat solver and save the result to the SAT table
-- work' :: (Ord d, Show a, Show d, Integral a, MonadTrans t1,
--            MonadState (t, M.Map (Config d) Satisfiable) (t1 IO)) =>
--          (Config d, Bool, Maybe (VProp d a)) -> t1 IO ()
work' :: (MonadTrans m
         , MonadState (St d) (m IO)
         , Ord d
         , Show d
         , Integral a) => (Config d, Maybe (VProp d a)) -> m IO ()
work' (conf, prop) = when (isJust prop) $
  do (vars, sats) <- get
     result <- lift . runPMinisat . propToCNF (show conf) . fmap fromJust . ground conf . fromJust $ prop
     put (vars, M.insert conf result sats)

-- This test case never terminates: run with (flip runEnv) Opts{baseline=False, others=[]} . initAndRun $ p1
-- (-(((2 -> 1) && (2 -> 1)) -> ((2 -> 2) || "ibyzldzishzdd"<1, 5>)) -> 1)
p1 :: VProp String Integer
p1 = _or (_impl
          (_and
           (_impl (Ref 2) (Ref 1))
           (_impl (Ref 2) (Ref 1)))
          (_impl
           (_or
            (_impl (Ref 2) (Ref 2))
             (Chc "a" (Ref 1) (Ref 5)))
           (Ref 1))) (Ref 2)

p2 :: VProp String Integer
p2 = _or (Ref 1) (Ref 2)
