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
import Data.Time.Clock
import Data.Time.Calendar

myConfig :: Config
myConfig = C.defaultConfig { resamples = 6 }

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
resDescFile :: FilePath
resDescFile = "andIncDesc.csv"

bfDescFile :: FilePath
bfDescFile = "desc_results.csv"

eraseFile :: FilePath -> IO ()
eraseFile = flip writeFile ""

main :: IO ()
main = do
  mapM_ eraseFile [resDescFile, bfDescFile]
  mapM_ benchAndInc $ zip [1..] [0..50] >>= replicate 6

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

      noShPropRecord = RunData "Unique" s rn n c p sd sp ms
      propRecord = RunData "Shared" s2 rn n c2 p2 sd2 sp2 ms2

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

  -- write out to descriptor csv file duplicated just to make the data frame
  -- merge in R easier
  appendFile bfDescFile $ encodeByName headers $ replicate 3 noShPropRecord
  appendFile bfDescFile $ encodeByName headers $ replicate 3 propRecord

  C.defaultMainWith myConfig
    [ C.bgroup ("Unique/" ++ show n)
      [ bench "Brute Force" $ C.nfIO (runEnv True False False [] noShProp)
      , bench "And Decomposition" $ C.nfIO (runEnv True True False [] noShProp)
      , bench "Variational Solve" $ C.nfIO (runEnv False False False [] noShProp)
      ]
    , C.bgroup ("Shared/" ++ show n)
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

      noShPropRecord = RunData "Unique" s rn n c p sd sp ms
      propRecord = RunData "Shared" s2 rn n c2 p2 sd2 sp2 ms2

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
  appendFile bfDescFile $ encodeByName headers $ pure noShPropRecord
  appendFile bfDescFile $ encodeByName headers $ pure propRecord

  C.defaultMainWith myConfig
    [ C.bgroup ("Unique/" ++ show n)
      [ bench "And Decomposition" $ C.nfIO (runEnv True True False [] noShProp)
      , bench "Variational Solve" $ C.nfIO (runEnv False False False [] noShProp)
      ]
    , C.bgroup ("Shared/" ++ show n)
      [ bench "And Decomposition" $ C.nfIO (runEnv True True False [] prop)
      , bench "Variational Solve" $ C.nfIO (runEnv False False False [] prop)
      ]
    ]
