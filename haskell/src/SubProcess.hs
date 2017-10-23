module SubProcess where

import qualified Turtle as T
import Turtle.Line
import qualified Data.Text as D (pack)
import Data.Maybe (fromJust)
import qualified Control.Foldl as F
import Data.List (groupBy, nub)
import Data.Function (on)

import CNF
import TagTree

type Satisfiable = Bool

-- | A result is a particular configuration, and its satisfiability result
type Result = (Config, Satisfiable)

-- | Take anything that can be shown and pack it into a shell line
toLine :: (Show a) => a -> T.Shell Line
toLine = T.select . textToLines . D.pack . show

-- | Given a Variational CNF generate a config for all choices
genConfig :: CNF Variational V -> [Config]
genConfig cnf = sequence $ groupBy ((==) `on` fst) configs
  where tags' = nub . concatMap tags . concat . filter (any isChc) $ clauses cnf
        configs = (,) <$> tags' <*> [True, False]

-- | Given a config and a Variational CNF, transform to a Plain CNF
toPlain :: Config -> CNF Variational V -> CNF Plain V
toPlain cs CNF{ comment = c
              , vars    = _
              ,clauses = cl
              } = new
  where new = CNF { comment = c
                  , vars = toVars new
                  , clauses = fmap (fmap $ one . fromJust . select cs) cl
                  }

-- | Take any Sat solver that can be called from shell, and a plain CNF term
-- and run the CNF through the specified SAT solver
run :: T.Text -> CNF a V -> IO Satisfiable
run sat cnf = do
  let output = T.inproc sat [] (toLine cnf)
      res = T.grep (T.has "UNSATISFIABLE") output
  res' <- T.fold res F.length
  return $ (/=1) res'

-- | take any Sat solver that can be called from shell, and any variational CNF
-- term, and run all combinations of the CNF through the SAT solver
runV :: T.Text -> CNF Variational V -> IO [Result]
runV solver cnf = do
  results <- sequence $ run solver <$> plains
  let returnVals = zip configs results
  return returnVals
  where
    configs :: [Config]
    configs = genConfig cnf

    plains :: [CNF Plain V]
    plains = flip toPlain cnf <$> configs


-- | Take any plain CNF term and run it through the SAT solver
runPMinisat :: CNF Plain V -> IO Satisfiable
runPMinisat = run "minisat"

-- | Take any variational CNF term and run it through the SAT solver
runVMinisat :: CNF Variational V -> IO [Result]
runVMinisat = runV "minisat"

-- | Given a list of results, only return the failures
failures :: [Result] -> [Result]
failures = filter ((==False) . snd)
