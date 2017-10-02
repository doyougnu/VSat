module SubProcess where

import qualified Turtle as T
import Turtle.Line
import Data.Text (pack)
import System.Posix.Process (getProcessID)

import CNF

toLine :: (Show a) => a -> T.Shell Line
toLine = T.select . textToLines . pack . show

-- runMinisat :: T.FilePath -> CNF -> IO Bool
runMinisat :: CNF -> IO (T.Shell Line)
runMinisat cnf = do
  let output = T.inproc "minisat" [] (toLine cnf)
      res = T.grep (T.has "restarts") output
  return res
