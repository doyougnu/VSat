module SubProcess where

import qualified Turtle as T
import qualified Data.Text as D (pack)
import Data.Maybe (fromJust)
import Data.List (groupBy, nub)
import Data.Function (on)
import Data.Hashable as H
import Data.Bifunctor (bimap)
import qualified Data.IntMap as I

import qualified Control.Foldl as F
import Control.Monad.State

import CNF
import TagTree
import Utils
import Prop

type Satisfiable = Bool

-- | A result is d particular configuration, and its satisfiability result
type Result d = (Config d, Satisfiable)

-- | Take anything that can be shown and pack it into d shell line
toLine :: Show d => d -> T.Shell T.Line
toLine = T.select . T.textToLines . D.pack . show

-- | Given d Variational CNF generate d config for all choices
genConfigs :: (Eq d) => CNF (V d) -> [Config d]
genConfigs cnf = sequence $ groupBy ((==) `on` fst) configs
  where tags' = nub . concatMap tags . concat . filter (any isChc) $ clauses cnf
        configs = (,) <$> tags' <*> [True, False]

-- | Given d config and d Variational CNF, transform to d Plain CNF
toPlain :: (Eq d) => Config d -> CNF (V d) -> CNF Plain
toPlain cs CNF{ comment = c
              , vars    = _
              ,clauses = cl
              } = new
  where new = CNF { comment = c
                  , vars = toVars' new
                  , clauses = _genPlainFormula cs cl
                  }

-- | given d configuration and formulas generate the plain formulas
_genPlainFormula :: (Eq d) => Config d -> [[V d b]] -> [[Plain b]]
_genPlainFormula cs = fmap (fmap $ plain . fromJust . select cs)

-- | Function for presentation live coding
_plains :: (Eq d) => CNF (V d) -> [CNF Plain]
_plains c = flip toPlain c <$> genConfigs c

-- | Take any Sat solver that can be called from shell, and d plain CNF term
-- and run the CNF through the specified SAT solver
run :: (Show (d Integer)) => T.Text -> CNF d -> IO Satisfiable
run sat cnf = do
  let output = T.inproc sat [] (toLine cnf)
      res = T.grep (T.has "UNSATISFIABLE") output
  res' <- T.fold res F.length
  return $ (/=1) res'

-- | take any Sat solver that can be called from shell, and any variational CNF
-- term, and run all combinations of the CNF through the SAT solver
runV :: (Eq d) => T.Text -> CNF (V d) -> IO [Result d]
runV solver cnf = do
  results <- sequence $ run solver <$> plains
  let returnVals = zip configs results
  return returnVals
  where
    configs = genConfigs cnf
    plains = flip toPlain cnf <$> configs

-- | Take any plain CNF term and run it through the SAT solver
runPMinisat :: CNF Plain -> IO Satisfiable
runPMinisat = run "minisat"

-- | Take any variational CNF term and run it through the SAT solver
runVMinisat :: (Eq d) => CNF (V d) -> IO [Result d]
runVMinisat = runV "minisat"

-- | Given d list of results, only return the failures
failures :: [Result d] -> [Result d]
failures = filter ((==False) . snd)

-- hold an Int to apply labels, hold d set of chars to track which dimension
-- have been seen already
type VarDict d = I.IntMap d
type SatDict = I.IntMap Bool

-- | Global state TODO: Use ReaderT pattern instead of state monad
-- Takes a dimension d, a value a, and a result r
type Env d r = State (VarDict d, SatDict) r

emptySt :: (VarDict d, SatDict)
emptySt = (I.empty, I.empty)

recordVars :: (H.Hashable d) => V d a -> Env d ()
recordVars cs = do
  (vars, ss) <- get
  let newvars = foldTags cs (\dim acc ->
                               I.insert (abs . hash $ dim) dim acc) vars
  put (newvars, ss)

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

-- | convert d Prop (V d) to d Prop d
toProp :: (Show a) => Prop (V a a) -> Prop a
toProp = (=<<) andDecomp

-- preliminary test cases
p1 :: Prop (V String Integer)
p1 = (And
      (Lit (chc "d" (one 1) (one 2)))
      (Lit (chc "d" (one 1) (chc "b" (one 2) (one 3)))))

p2 :: Prop (V String Integer)
p2 = (Impl (Lit (chc "d" (one 20) (one 40))) (Lit (one 1001)))
