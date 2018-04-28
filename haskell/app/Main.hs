module Main where

import Criterion.Main as C

import Run
import Criterion.Types
import Data.Csv
import qualified Data.Text as T
import qualified Data.Vector as V
import Prelude hiding (writeFile, appendFile)
import Data.ByteString.Char8 (pack)
import Data.ByteString.Lazy (writeFile, appendFile, empty)
import VProp ( VProp
             , Readable
             , readStr
             , vPropNoShare
             , mkLargeVProp
             , genVProp
             , numTerms
             , numChc
             , numPlain
             , numSharedDims
             , numSharedPlain
             , maxShared
             )
import Test.QuickCheck (generate, arbitrary)

myConfig = C.defaultConfig { resamples = 10 }

data RunData = RunData { scale          :: !T.Text
                       , numTerms       :: !T.Text
                       , numChc         :: !T.Text
                       , numPlain       :: !T.Text
                       , numSharedPlain :: !T.Text
                       , maxShared      :: !T.Text
                       }

-- run with $ stack bench --benchmark-arguments "--output <benchmark-file>.html"
main :: IO ()
main = mapM_ benchAndInc [1..2]

benchAll n = do
  noShProp <- fmap readStr <$> (generate $ mkLargeVProp n vPropNoShare :: IO (VProp Readable))
  prop <- fmap readStr <$> (generate $ mkLargeVProp n arbitrary :: IO (VProp Readable))
  let descriptorsFs = [numTerms, numChc, numPlain, numSharedDims, numSharedPlain, maxShared]
      descriptorsNoSh = show n : (fmap show $ descriptorsFs <*> pure noShProp)
      descriptors = show n : (fmap show $ descriptorsFs <*> pure prop)

      headers :: Header
      headers = V.fromList $ pack <$> [show n, "numTerms", "numChc", "numPlain", "numSharedDims", "numSharedPlain", "maxShared"]

  writeFile "test_Desc.csv" ""
  appendFile "test_Desc.csv" $ headers
  appendFile "test_Desc.csv" $ encodeByName $ fmap pack descriptorsNoSh
  appendFile "test_Desc.csv" $ encode $ fmap pack descriptors

  C.defaultMainWith myConfig
    [ C.bgroup ("Baselines, Unique Dimensions, scaled: " ++ show n)
      [ bench "Brute Force" $ C.nfIO (runEnv True False False [] noShProp)
      , bench "And Decomposition" $ C.nfIO (runEnv True True False [] noShProp)
      , bench "Variational Solve" $ C.nfIO (runEnv False False False [] noShProp)
      ]
    , C.bgroup ("Baselines, scaled: " ++ show n)
      [ bench "Brute Force" $ C.nfIO (runEnv True False False [] prop)
      , bench "And Decomposition" $ C.nfIO (runEnv True True False [] prop)
      , bench "Variational Solve" $ C.nfIO (runEnv False False False [] prop)
      ]
    ]

benchAndInc n = do
  noShProp <- fmap readStr <$> (generate $ mkLargeVProp n vPropNoShare :: IO (VProp Readable))
  prop <- fmap readStr <$> (generate $ mkLargeVProp n arbitrary :: IO (VProp Readable))
  let descriptors = [numTerms, numChc, numPlain, numSharedDims, numSharedPlain, maxShared]
      headers = [show n, "numTerms", "numChc", "numPlain", "numSharedDims", "numSharedPlain", "maxShared"]

  writeFile "test_Desc.csv" empty
  appendFile "test_Desc.csv" $ encode headers
  appendFile "test_Desc.csv" $ encode $ show n : (fmap show $ descriptors <*> pure noShProp)
  appendFile "test_Desc.csv" $ encode $ show n :  (fmap show $ descriptors <*> pure prop)

  C.defaultMainWith myConfig
    [ C.bgroup ("Unique Dimensions, scaled: " ++ show n)
      [ bench "And Decomposition" $ C.nfIO (runEnv True True False [] noShProp)
      , bench "Variational Solve" $ C.nfIO (runEnv False False False [] noShProp)
      ]
    , C.bgroup ("Shared Dimensions, scaled: " ++ show n)
      [ bench "And Decomposition" $ C.nfIO (runEnv True True False [] prop)
      , bench "Variational Solve" $ C.nfIO (runEnv False False False [] prop)
      ]
    ]
