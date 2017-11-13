module SubProcess where

import qualified Turtle as T
import Turtle.Line
import qualified Data.Text as D (pack)
import Data.Maybe (fromJust)
import qualified Control.Foldl as F
import Data.List (groupBy, nub)
import Data.Function (on)
import Control.Monad.State

import CNF
import TagTree
import Utils

type Satisfiable = Bool

-- | A result is a particular configuration, and its satisfiability result
type Result a = (Config a, Satisfiable)

-- | Take anything that can be shown and pack it into a shell line
toLine :: Show a => a -> T.Shell Line
toLine = T.select . textToLines . D.pack . show

-- | Given a Variational CNF generate a config for all choices
genConfigs :: (Eq a) => CNF (V a) -> [Config a]
genConfigs cnf = sequence $ groupBy ((==) `on` fst) configs
  where tags' = nub . concatMap tags . concat . filter (any isChc) $ clauses cnf
        configs = (,) <$> tags' <*> [True, False]

-- | Given a config and a Variational CNF, transform to a Plain CNF
toPlain :: (Eq a) => Config a -> CNF (V a) -> CNF Plain
toPlain cs CNF{ comment = c
              , vars    = _
              ,clauses = cl
              } = new
  where new = CNF { comment = c
                  , vars = toVars' new
                  , clauses = _genPlainFormula cs cl
                  }

-- given a configuration and formulas generate the plain formulas
_genPlainFormula :: (Eq a) => Config a -> [[V a b]] -> [[Plain b]]
_genPlainFormula cs = fmap (fmap $ plain . fromJust . select cs)

-- | Function for presentation live coding
_plains :: (Eq a) => CNF (V a) -> [CNF Plain]
_plains c = flip toPlain c <$> genConfigs c

-- | Take any Sat solver that can be called from shell, and a plain CNF term
-- and run the CNF through the specified SAT solver
run :: (Show (a Integer)) => T.Text -> CNF a -> IO Satisfiable
run sat cnf = do
  let output = T.inproc sat [] (toLine cnf)
      res = T.grep (T.has "UNSATISFIABLE") output
  res' <- T.fold res F.length
  return $ (/=1) res'

-- | take any Sat solver that can be called from shell, and any variational CNF
-- term, and run all combinations of the CNF through the SAT solver
runV :: (Eq a) => T.Text -> CNF (V a) -> IO [Result a]
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
runVMinisat :: (Eq a) => CNF (V a) -> IO [Result a]
runVMinisat = runV "minisat"

-- | Given a list of results, only return the failures
failures :: [Result a] -> [Result a]
failures = filter ((==False) . snd)

-- | increment the simple counter state
inc :: State Int ()
inc = get >>= put . succ

-- | crawl a tag tree and label each choice node with the current count
_count :: V a b -> State Int (V (Int, a) b)
_count (Obj a) = return (Obj a)
_count (Chc d l r) = do
  inc
  n <- get
  l' <- _count l
  r' <- _count r
  return $ (Chc (n, d) l' r')

-- | crawl a tag tree and label each choice with a unique integer
label :: V a b -> State Int (V Int b)
label (Obj a) = return (Obj a)
label (Chc _ l r) = do
  inc
  n <- get
  l' <- label l
  r' <- label r
  return $ (Chc n l' r')
