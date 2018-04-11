module Main where

import Run
import VProp (vPropNoShare, mkLargeVProp, genVProp, maxShared)
import Test.QuickCheck (generate, arbitrary)

main :: IO (Result, SatDict, Log)
main = do
  noShPrp <- generate vPropNoShare
  runEnv True False False [] noShPrp
