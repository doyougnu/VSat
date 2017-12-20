module SubProcess where

import qualified Turtle as T
import qualified Data.Text as D (pack)
-- import Data.Maybe (fromJust)
-- import Data.List (groupBy, nub)
-- import Data.Function (on)
import Data.Hashable as H
import Data.Bifunctor (bimap)
import qualified Data.IntMap as I
import qualified Data.Set as S (fromList)

import qualified Control.Foldl as F
import Control.Monad.State

import CNF
import TagTree
import Prop

type Satisfiable = Bool

-- | A result is d particular configuration, and its satisfiability result
type Result d = (Config d, Satisfiable)

-- | Take anything that can be shown and pack it into d shell line
toLine :: Show d => d -> T.Shell T.Line
toLine = T.select . T.textToLines . D.pack . show

-- | Take any Sat solver that can be called from shell, and d plain CNF term
-- and run the CNF through the specified SAT solver
run :: T.Text -> CNF -> IO Satisfiable
run sat cnf = do
  let output = T.inproc sat [] (toLine cnf)
      res = T.grep (T.has "UNSATISFIABLE") output
  res' <- T.fold res F.length
  return $ (/=1) res'

-- | Take any plain CNF term and run it through the SAT solver
runPMinisat :: CNF -> IO Satisfiable
runPMinisat = run "minisat"

-- | Given d list of results, only return the failures
failures :: [Result d] -> [Result d]
failures = filter ((==False) . snd)

-- hold an Int to apply labels, hold d set of chars to track which dimension
-- have been seen already
type VarDict d = I.IntMap d
type SatDict = I.IntMap Satisfiable

-- | Global state TODO: Use ReaderT pattern instead of state monad
-- Takes a dimension d, a value a, and a result r
type Env d r = StateT (VarDict d, SatDict) IO r

runEnv :: StateT (VarDict d, SatDict) m a -> m (a, (VarDict d, SatDict))
runEnv m = runStateT m emptySt

-- | An Empty env state is a dictionary of variable names and their hashes and
-- a dictionary for each hash that holds the results of the sat solver
emptySt :: (VarDict d, SatDict)
emptySt = (I.empty, I.empty)

-- | Given a variational term pack an initial state in the environment Monad
recordVars :: (H.Hashable d) => V d a -> Env d ()
recordVars cs = do
  st <- get
  -- TODO: use a let binding for the hashed dimension variable
  let (newvars, newss) = foldTags cs (\dim (_new_vars, _new_ss) ->
                               (I.insert (abs . hash $ dim) dim _new_vars
                               , I.insert (negate . hash $ dim) False $
                                 I.insert (hash dim) False _new_ss)) st
  put (newvars, newss)

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
initEnv :: (H.Hashable d, Integral a) => Prop (V d a) -> Env d (Prop Integer)
initEnv cs = do
  forM_ cs recordVars
  cs' <- toProp cs
  let cnf = propToCNF "does it run?" cs'
  lift $ runPMinisat cnf >>= putStrLn . show
  return cs'


-- | convert  propositional term to a DIMACS CNF term
propToCNF :: (Num a, Integral a) => String -> Prop a -> CNF
propToCNF str ps = genVars cnf
  where
    cnf = CNF { comment = str
              , vars    = S.fromList [0]
              , clauses = orSplit . toListAndSplit . ground $ toInteger <$> ps
              }

-- -- | main workhorse for running the SAT solver
-- work :: Env d (Prop Integer)
-- work = do

-- preliminary test cases
p1 :: Prop (V String Integer)
p1 = And
      (Lit (chc "d" (one 1) (one 2)))
      (Lit (chc "d" (one 1) (chc "b" (one 2) (one 3))))

p2 :: Prop (V String Integer)
p2 = Impl (Lit (chc "d" (one 20) (one 40))) (Lit (one 1001))
