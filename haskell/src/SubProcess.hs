module SubProcess where

import qualified Turtle as T
import Turtle.Line
import Data.Text (pack)
import System.Posix.Process (getProcessID)

import CNF

-- | Take anything that can be shown and pack it into a shell line
toLine :: (Show a) => a -> T.Shell Line
toLine = T.select . textToLines . pack . show

-- | Take any Sat solver that can be called from shell, and a plain CNF term
-- and run the CNF through the specified SAT solver
run :: T.Text -> CNF Plain -> IO ()
run sat cnf = do
  let output = T.inproc sat [] (toLine cnf)
      res = T.grep (T.has "SATISFIABLE") output
  T.view res

-- | Take any plain CNF term and run it through the SAT solver
-- Run like: runMinisat $ toPlain [(3, True)] vEx1
runMinisat :: CNF Plain -> IO ()
runMinisat = run "minisat"
