module Main where

import Criterion.Main as C

import Run
import Criterion.Types
import Data.Csv
import Data.Text (Text)
import qualified Data.Vector as V
import Prelude hiding (writeFile, appendFile)
import GHC.Generics (Generic)
import Data.ByteString.Char8 (pack)
import Data.ByteString.Lazy (writeFile, appendFile)
import VProp ( VProp
             , Readable
             , readStr
             , vPropNoShare
             , mkLargeVProp
             , numTerms
             , numChc
             , numPlain
             , numSharedDims
             , numSharedPlain
             , maxShared
             )
import Test.QuickCheck (generate, arbitrary)
-- import Data.Time.Clock
-- import Data.Time.Calendar

myConfig :: Config
myConfig = C.defaultConfig { resamples = 2 }

-- | Required field namings for cassava csv library
data RunData = RunData { shared_         :: !Text
                       , runNum_          :: !Integer
                       , scale_          :: !Integer
                       , numTerms_       :: !Integer
                       , numChc_         :: !Integer
                       , numPlain_       :: !Integer
                       , numSharedDims_  :: !Integer
                       , numSharedPlain_ :: !Integer
                       , maxShared_      :: !Integer
                       } deriving (Generic, Show)

instance ToNamedRecord RunData

-- run with $ stack bench --benchmark-arguments "--output results.html --csv timing_results.csv"
descFile :: FilePath
descFile = "desc_results.csv"

timingFile :: FilePath
timingFile = "timing_results.csv"


eraseFile :: FilePath -> IO ()
eraseFile = flip writeFile ""

main :: IO ()
main = do
  mapM_ eraseFile [descFile, timingFile]
  mapM_ benchAndInc $ zip [1..] $ [100,120..1000] >>= replicate 5

-- | The run number, used to join descriptor and timing data later
type RunNum = Integer

benchAll :: (RunNum, Integer) -> IO ()
benchAll (rn, n) = do
  noShProp <- fmap readStr <$>
              (generate $ mkLargeVProp (fromInteger n) vPropNoShare :: IO (VProp Readable))
  prop <- fmap readStr <$>
          (generate $ mkLargeVProp (fromInteger n) arbitrary :: IO (VProp Readable))
  let descriptorsFs = [ numTerms
                      , numChc
                      , numPlain
                      , numSharedDims
                      , numSharedPlain
                      , maxShared
                      ]

  -- there must be a better way
      [s,c,p,sd,sp,ms] = descriptorsFs <*> pure noShProp
      [s2,c2,p2,sd2,sp2,ms2] = descriptorsFs <*> pure prop

      noShPropRecord = RunData "Unique" rn n s  c  p  sd  sp  ms
      propRecord =     RunData "Shared" rn n s2 c2 p2 sd2 sp2 ms2

      headers :: Header
      headers = V.fromList $ pack <$>
                [ "shared_"
                , "runNum_"
                , "scale_"
                , "numTerms_"
                , "numChc_"
                , "numPlain_"
                , "numSharedDims_"
                , "numSharedPlain_"
                , "maxShared_"
                ]

  -- write out to descriptor csv file
  appendFile descFile $ encodeByName headers $ pure noShPropRecord
  appendFile descFile $ encodeByName headers $ pure propRecord

  C.defaultMainWith myConfig
    [ C.bgroup ("Unique/" ++ show rn ++ "/" ++ show n)
      [ bench "Brute Force" $ C.nfIO (runEnv True False False [] noShProp)
      , bench "And Decomposition" $ C.nfIO (runEnv True True False [] noShProp)
      , bench "Variational Solve" $ C.nfIO (runEnv False False False [] noShProp)
      ]
    , C.bgroup ("Shared/" ++ show rn ++ "/" ++ show n)
      [ bench "Brute Force" $ C.nfIO (runEnv True False False [] prop)
      , bench "And Decomposition" $ C.nfIO (runEnv True True False [] prop)
      , bench "Variational Solve" $ C.nfIO (runEnv False False False [] prop)
      ]
    ]


benchAndInc :: (RunNum, Integer) -> IO ()
benchAndInc (rn, n) = do
  noShProp <- fmap readStr <$>
              (generate $ mkLargeVProp (fromInteger n) vPropNoShare :: IO (VProp Readable))
  prop <- fmap readStr <$>
          (generate $ mkLargeVProp (fromInteger n) arbitrary :: IO (VProp Readable))
  let descriptorsFs = [ numTerms
                      , numChc
                      , numPlain
                      , numSharedDims
                      , numSharedPlain
                      , maxShared
                      ]

  -- there must be a better way
      [s,c,p,sd,sp,ms] = descriptorsFs <*> pure noShProp
      [s2,c2,p2,sd2,sp2,ms2] = descriptorsFs <*> pure prop

      noShPropRecord = RunData "Unique" rn n s  c  p  sd  sp  ms
      propRecord =     RunData "Shared" rn n s2 c2 p2 sd2 sp2 ms2

      headers :: Header
      headers = V.fromList $ pack <$>
                [ "shared_"
                , "runNum_"
                , "scale_"
                , "numTerms_"
                , "numChc_"
                , "numPlain_"
                , "numSharedDims_"
                , "numSharedPlain_"
                , "maxShared_"
                ]

  -- write out to descriptor csv file
  appendFile descFile $ encodeByName headers $ pure noShPropRecord
  appendFile descFile $ encodeByName headers $ pure propRecord

  C.defaultMainWith myConfig
    [ C.bgroup ("Unique/" ++ show rn ++ "/" ++ show n)
      [ bench "And Decomposition" $ C.nfIO (runEnv True True False [] noShProp)
      , bench "Variational Solve" $ C.nfIO (runEnv False False False [] noShProp)
      ]
    , C.bgroup ("Shared/" ++ show rn ++ "/" ++ show n)
      [ bench "And Decomposition" $ C.nfIO (runEnv True True False [] prop)
      , bench "Variational Solve" $ C.nfIO (runEnv False False False [] prop)
      ]
    ]
