module SubProcess where

import Turtle
import Turtle.Line
import Data.Text (pack)
import System.Posix.Process (getProcessID)

import CNF

toLine :: CNF -> Shell Line
toLine = select . textToLines . pack . show

mkTmpFileName :: Shell Line
mkTmpFileName = undefined
