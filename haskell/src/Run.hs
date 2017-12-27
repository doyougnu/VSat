module Run where


import Data.Hashable as H
import Data.Bifunctor (bimap)
import Data.Bifoldable
import qualified Data.IntMap as I
import qualified Data.Map as M
import qualified Data.Set as S (fromList)
import Control.Monad.RWS.Lazy

import Prop
import TagTree
import CNF
import SubProcess

-- hold an Int to apply labels, hold d set of chars to track which dimension
-- have been seen already
type VarDict d = I.IntMap d
type SatDict d = M.Map [(d, Bool)] Satisfiable
data Opts a = Opts { baseline :: Bool -- ^ True for andDecomp, False for brute
                   , others :: [Prop a -> Prop a] -- ^ a list of optimizations
                   }
type Log = String

-- | Global state TODO: Use ReaderT pattern instead of state monad
-- Takes a dimension d, a value a, and a result r
type Env d r = RWST (Opts r) Log (VarDict d, SatDict d) IO r


-- | An empty reader monad environment, in the future read these from config file
emptyOpts :: Opts a
emptyOpts = Opts { baseline = True -- set to use andDecomp
                 , others = []
                 }

-- | Run the RWS monad with defaults of empty state, reader
runEnv :: Env d r -> IO (r, (VarDict d, SatDict d),  Log)
runEnv m = runRWST m emptyOpts emptySt

-- | An Empty env state is a dictionary of variable names and their hashes and
-- a dictionary for each hash that holds the results of the sat solver
emptySt :: (VarDict d, SatDict d)
emptySt = (I.empty, M.empty)

-- | Given a variational term pack an initial state in the environment Monad
recordVars :: (H.Hashable d, MonadState (VarDict d, SatDict d) m) => V d a -> m ()
recordVars cs = do
  st <- get
  -- let newvars = bifoldr cs (\dim (vars, sats) ->
  --                              (I.insert (abs . hash $ dim) dim vars
  --                              , fd)) st
  let newvars =
        bifoldr
        (\dim (vars, sats) -> (I.insert (abs . hash $ dim) dim vars, sats))
        (\_ s -> s) st cs
  put newvars

-- | Unify the dimension and value in d choice to the same type using bifunctor
-- add all dimensions and their hashes to the variable dictionary
unify :: (Integral a, H.Hashable d) => V d a -> V Integer Integer
unify = bimap (toInteger . abs . hash) toInteger

-- | And Decomposition, convert choices to propositional terms
andDecomp :: (Show a) => V a a -> Prop a
andDecomp (Chc t l r) = Or
                        (And (Lit t)       (andDecomp l))
                        (And (Neg $ Lit t) (andDecomp r))
andDecomp (Obj x)     = Lit x

-- | orient the state monad to run the sat solver
toProp :: (H.Hashable d, Integral a, Monad m) => Prop (V d a) -> m (Prop Integer)
toProp cs = return $ cs >>= (andDecomp . unify)

-- | given a variational prop term iterate over the choices, pack the initial
-- environment, then convert the choices to a plain prop term using andDecomp
-- TODO: Disentangle this init and work coupling, want initEnv >=> work pipeline
initEnv :: (H.Hashable d, Integral a) => Prop (V d a) -> Env d (Prop Integer)
initEnv cs = do
  forM_ cs recordVars
  work cs

-- | convert  propositional term to a DIMACS CNF term
propToCNF :: (Num a, Integral a) => String -> GProp a -> CNF
propToCNF str ps = genVars cnf
  where
    cnf = CNF { comment = str
              , vars    = S.fromList [0]
              , clauses = orSplit . toListAndSplit $ toInteger <$> ps
              }

-- | main workhorse for running the SAT solver
work :: (Hashable d, Integral a) => Prop (V d a) -> Env d (Prop Integer)
work cs = do
  cs' <- toProp cs
  let cnf = propToCNF "does it run?" $ ground cs'
  lift $ runPMinisat cnf >>= putStrLn . show
  return cs'

-- preliminary test cases run with: runEnv (initEnv p1)
p1 :: Prop (V String Integer)
p1 = And
      (Lit (chc "d" (one 1) (one 2)))
      (Lit (chc "d" (one 1) (chc "b" (one 2) (one 3))))

p2 :: Prop (V String Integer)
p2 = Impl (Lit (chc "d" (one 20) (one 40))) (Lit (one 1001))
