module Main where

import Run
import VProp (vPropNoShare, mkLargeVProp, genVProp, maxShared)
import Test.QuickCheck (generate, arbitrary)

main :: IO ()
main = do
  noShPrp <- generate vPropNoShare
  res <- runEnv True False False [] noShPrp
  putStrLn $ show res
  return ()
