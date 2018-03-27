import Criterion.Main as C

import Run
import VProp (vPropNoShare, mkLargeVProp, genVProp, maxShared)
import Test.QuickCheck (generate, arbitrary)

-- run with $ stack bench --benchmark-arguments "--output <benchmark-file>.html"
main :: IO ()
main = do
  noShProp <- generate vPropNoShare
  prop <- genVProp
  noShLrgeProp <- generate $ mkLargeVProp vPropNoShare
  largeProp <- generate $ mkLargeVProp arbitrary
  print $ maxShared largeProp
  C.defaultMain
    [ C.bgroup "Baselines, no Sharing"
      [ bench "Brute Force" $ C.nfIO (runEnv True False False [] noShProp)
      , bench "And Decomposition" $ C.nfIO (runEnv True True False [] noShProp)
      , bench "Variational Solve" $ C.nfIO (runEnv False False False [] noShProp)
      ]
    , C.bgroup "Baselines, Sharing"
      [ bench "Brute Force" $ C.nfIO (runEnv True False False [] prop)
      , bench "And Decomposition" $ C.nfIO (runEnv True True False [] prop)
      , bench "Variational Solve" $ C.nfIO (runEnv False False False [] prop)
      ]
    , C.bgroup "Large Terms, No Sharing"
      [ bench "Brute Force" $ C.nfIO (runEnv True False False [] noShLrgeProp)
      , bench "And Decomposition" $ C.nfIO (runEnv True True False [] noShLrgeProp)
      , bench "Variational Solve" $ C.nfIO (runEnv False False False [] noShLrgeProp)
      ]
    , C.bgroup "Large Terms, Sharing"
      [ bench "Brute Force" $ C.nfIO (runEnv True False False [] largeProp)
      , bench "And Decomposition" $ C.nfIO (runEnv True True False [] largeProp)
      , bench "Variational Solve" $ C.nfIO (runEnv False False False [] largeProp)
      ]
    ]
