import Criterion.Main as C

import Run
import Criterion.Types
import Data.Csv
import qualified Data.Text as T
import qualified Data.Vector as V
import Prelude hiding (writeFile, appendFile)
import GHC.Generics (Generic)
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

data RunData = RunData { shared_         :: !Bool
                       , scale_          :: !Integer
                       , numTerms_       :: !Integer
                       , numChc_         :: !Integer
                       , numPlain_       :: !Integer
                       , numSharedDims_  :: !Integer
                       , numSharedPlain_ :: !Integer
                       , maxShared_      :: !Integer
                       } deriving (Generic, Show)

instance ToField Bool where
  toField = pack . show

instance ToNamedRecord RunData

-- run with $ stack bench --benchmark-arguments "--output <benchmark-file>.html"
main :: IO ()
main = do
  writeFile "test_Desc.csv" ""
  mapM_ benchAndInc [1..2]

benchAll n = do
  noShProp <- fmap readStr <$>
              (generate $ mkLargeVProp n vPropNoShare :: IO (VProp Readable))
  prop <- fmap readStr <$>
          (generate $ mkLargeVProp n arbitrary :: IO (VProp Readable))
  let descriptorsFs = [ numTerms
                      , numChc
                      , numPlain
                      , numSharedDims
                      , numSharedPlain
                      , maxShared
                      ]

  -- there must be a better way
      [s,t,c,p,sd,sp,ms] = toInteger n : (descriptorsFs <*> pure noShProp)
      [s2,t2,c2,p2,sd2,sp2,ms2] = toInteger n : (descriptorsFs <*> pure prop)

      noShPropRecord = RunData False s t c p sd sp ms
      propRecord = RunData True s2 t2 c2 p2 sd2 sp2 ms2

      headers :: Header
      headers = V.fromList $ pack <$>
                [ "shared_"
                , "scale_"
                , "numTerms_"
                , "numChc_"
                , "numPlain_"
                , "numSharedDims_"
                , "numSharedPlain_"
                , "maxShared_"
                ]

  -- write out to descriptor csv file
  appendFile "test_Desc.csv" $ encodeByName headers $ pure noShPropRecord
  appendFile "test_Desc.csv" $ encodeByName headers $ pure propRecord

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
  noShProp <- fmap readStr <$>
              (generate $ mkLargeVProp n vPropNoShare :: IO (VProp Readable))
  prop <- fmap readStr <$>
          (generate $ mkLargeVProp n arbitrary :: IO (VProp Readable))
  let descriptorsFs = [ numTerms
                      , numChc
                      , numPlain
                      , numSharedDims
                      , numSharedPlain
                      , maxShared
                      ]

  -- there must be a better way
      [s,t,c,p,sd,sp,ms] = toInteger n : (descriptorsFs <*> pure noShProp)
      [s2,t2,c2,p2,sd2,sp2,ms2] = toInteger n : (descriptorsFs <*> pure prop)

      noShPropRecord = RunData False s t c p sd sp ms
      propRecord = RunData True s2 t2 c2 p2 sd2 sp2 ms2

      headers :: Header
      headers = V.fromList $ pack <$>
                [ "shared_"
                , "scale_"
                , "numTerms_"
                , "numChc_"
                , "numPlain_"
                , "numSharedDims_"
                , "numSharedPlain_"
                , "maxShared_"
                ]

  -- write out to descriptor csv file
  appendFile "test_Desc.csv" $ encodeByName headers $ pure noShPropRecord
  appendFile "test_Desc.csv" $ encodeByName headers $ pure propRecord

  C.defaultMainWith myConfig
    [ C.bgroup ("Baselines, Unique Dimensions, scaled: " ++ show n)
      [ bench "And Decomposition" $ C.nfIO (runEnv True True False [] noShProp)
      , bench "Variational Solve" $ C.nfIO (runEnv False False False [] noShProp)
      ]
    , C.bgroup ("Baselines, scaled: " ++ show n)
      [ bench "And Decomposition" $ C.nfIO (runEnv True True False [] prop)
      , bench "Variational Solve" $ C.nfIO (runEnv False False False [] prop)
      ]
    ]
