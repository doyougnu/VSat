module SubProcess (runPMinisat
                  , Satisfiable) where

import qualified Turtle as T
import qualified Data.Text as D (pack)
import qualified Control.Foldl as F

import CNF
import TagTree

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
