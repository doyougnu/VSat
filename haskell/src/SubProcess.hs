module SubProcess where

import qualified Turtle as T
import Turtle.Line
import Data.Text (pack)
import System.Posix.Process (getProcessID)

import CNF

toLine :: CNF -> T.Shell Line
toLine = T.select . textToLines . pack . show

tmpFileName :: T.FilePath
tmpFileName = ".tmp"

runMinisat :: T.FilePath -> CNF -> IO T.ExitCode
runMinisat f cnf = do
  T.output f (toLine cnf)
  exit <- T.proc "minisat" [T.format T.fp f] T.empty
  T.rm f
  return exit
