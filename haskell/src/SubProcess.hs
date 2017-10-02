module SubProcess where

import Turtle
import Turtle.Line
import Data.Text (pack)
import System.Posix.Process (getProcessID)

import CNF

toFile :: CNF -> IO ExitCode
toFile cnf = undefined

mkTmpFileName :: Shell Line
mkTmpFileName = undefined
