module Main where

import Run
import VProp (vPropNoShare)
import Test.QuickCheck (generate)

main :: IO (Result, SatDict String, Log)
main = do
  noShPrp <- generate vPropNoShare
  runEnv True False False [] noShPrp
