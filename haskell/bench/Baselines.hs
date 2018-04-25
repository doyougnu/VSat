import Criterion.Main as C

import Run
import Criterion.Types
import VProp (VProp, Readable, readStr, vPropNoShare, mkLargeVProp, genVProp, maxShared)
import Test.QuickCheck (generate, arbitrary)

myConfig = C.defaultConfig { resamples = 1 }

-- run with $ stack bench --benchmark-arguments "--output <benchmark-file>.html"
main :: IO ()
main = do
  noShProp <- fmap readStr <$> (generate vPropNoShare :: IO (VProp Readable))
  prop <- fmap readStr <$> (genVProp :: IO (VProp Readable))
  noShLrgeProp <- fmap readStr <$> (generate $ mkLargeVProp 1 vPropNoShare :: IO (VProp Readable))
  largeProp <- fmap readStr <$> (generate $ mkLargeVProp 1 arbitrary :: IO (VProp Readable))
  print $ "no Share Large Prop:    "
  print noShLrgeProp

  print $ "Large Prop:    "
  print largeProp
  C.defaultMainWith myConfig
    [ C.bgroup "Baselines, Unique Dimensions"
      [ bench "Brute Force" $ C.nfIO (runEnv True False False [] noShProp)
      , bench "And Decomposition" $ C.nfIO (runEnv True True False [] noShProp)
      , bench "Variational Solve" $ C.nfIO (runEnv False False False [] noShProp)
      ]
    , C.bgroup "Baselines"
      [ bench "Brute Force" $ C.nfIO (runEnv True False False [] prop)
      , bench "And Decomposition" $ C.nfIO (runEnv True True False [] prop)
      , bench "Variational Solve" $ C.nfIO (runEnv False False False [] prop)
      ]
    , C.bgroup "Large Terms, Unique Dimensions"
      [ bench "Brute Force" $ C.nfIO (runEnv True False False [] noShLrgeProp)
      , bench "And Decomposition" $ C.nfIO (runEnv True True False [] noShLrgeProp)
      , bench "Variational Solve" $ C.nfIO (runEnv False False False [] noShLrgeProp)
      ]
    , C.bgroup "Large Terms"
      [ bench "Brute Force" $ C.nfIO (runEnv True False False [] largeProp)
      , bench "And Decomposition" $ C.nfIO (runEnv True True False [] largeProp)
      , bench "Variational Solve" $ C.nfIO (runEnv False False False [] largeProp)
      ]
    ]
