import Run
import Data.Csv
import Data.Text (Text,pack,unpack)
import qualified Data.Vector as V
import Prelude hiding (writeFile, appendFile)
import GHC.Generics (Generic)
import qualified Data.ByteString.Char8 as BS (pack)
import Data.ByteString.Lazy (writeFile, appendFile)
import VProp.Core
import VProp.Types
import VProp.Gen
import Test.Tasty.QuickCheck (generate, choose)
import Control.DeepSeq (NFData)

import Api
import Config

import System.CPUTime
import System.Environment
import System.Timeout

import Data.Time.Clock
import Data.Time.Calendar

-- | Required field namings for cassava csv library
data RunData = RunData { shared_         :: !Text
                       , runNum_         :: !Int
                       , scale_          :: !Int
                       , numTerms_       :: !Integer
                       , numChc_         :: !Integer
                       , numPlain_       :: !Integer
                       , numSharedDims_  :: !Integer
                       , numSharedPlain_ :: !Integer
                       , maxShared_      :: !Integer
                       } deriving (Generic, Show)

data TimeData = TimeData { name__   :: !Text
                         , runNum__ :: !Int
                         , scale__  :: !Int
                         , time__   :: !Double
                         } deriving (Generic,Show)

instance ToNamedRecord RunData
instance ToNamedRecord TimeData
instance ToRecord RunData
instance ToRecord TimeData

eraseFile :: FilePath -> IO ()
eraseFile = flip writeFile ""

blankDescRow :: [RunData]
blankDescRow = []

blankTimeRow :: [TimeData]
blankTimeRow = []

-- run with stack build; stack bench --benchmark-arguments 'data timing_results desc_results'
main :: IO ()
main = do
  (folder:timingFile_:descFile_:_) <- getArgs

  -- format the filenames and clear the files
  timingFile <- format timingFile_ folder
  descFile   <- format descFile_ folder
  mapM_ eraseFile [descFile, timingFile]

  -- place headers on file
  appendFile descFile $ encodeByName descHeaders blankDescRow
  appendFile timingFile $ encodeByName timeHeaders blankTimeRow

  -- run the benchmark
  mapM_ (benchRandomSample descFile timingFile)
    $ zip [1..] $ [1,2..10] >>= replicate 10

-- | The run number, used to join descriptor and timing data later
type RunNum = Int

-- | The Term size used to generate an arbitrary VProp of size TermSize
type TermSize = Int

-- | The Run Number and TermSize used to generate the prop for the run
type RunMetric = (RunNum, TermSize)

descHeaders = V.fromList $ BS.pack <$>
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

timeHeaders = V.fromList $ BS.pack <$>
              ["name__", "runNum__", "scale__", "time__"]

-- | Give a descriptor, run metrics, and a prop, generate the descriptor metrics
-- for the prop and write them out to a csv
writeDesc :: (Show a, Show b, Eq a, Eq b) =>
  String -> RunMetric -> VProp a b -> FilePath -> IO ()
writeDesc desc (rn, n) prop' descFile = do
  let descriptorsFs = [ numTerms
                      , numChc
                      , numPlain
                      , numSharedDims
                      , numSharedPlain
                      , toInteger . maxShared
                      ]
      prop = show <$> prop'
      [s,c,p,sd,sp,ms] = descriptorsFs <*> pure prop
      row = RunData (pack desc) rn n s c p sd sp ms
  appendFile descFile . encode $ pure row

-- | Given a file path, get the year, date and time of the run and prepend it to
-- the filepath
prependDate :: FilePath -> IO FilePath
prependDate str = do (year, month, day) <- getCurrentTime >>=
                                           return . toGregorian . utctDay
                     return $ mconcat [show year, "-", show month
                                      , "-", show day, "-", str]

format :: FilePath -> FilePath -> IO FilePath
format fp fldr = prependDate (fp ++ ".csv") >>= return . ((++) (fldr ++ "/"))

-- | Given some string, a run metric (the parameters for the run) a time and a
-- file path, perform the IO effect
writeTime :: Text -> RunMetric -> Double -> FilePath -> IO ()
writeTime str (rn, n) time_ timingFile = appendFile timingFile . encode $ pure row
  where row = TimeData str rn n time_

-- | Given run metrics, benchmark a data where the frequency of terms is
-- randomly distributed from 0 to 10, dimensions and variables are sampled from
-- a bound pool so sharing is also very possible.
benchRandomSample :: FilePath -> FilePath -> RunMetric -> IO ()
benchRandomSample descfp timefp metrics@(_, n) = do
  prop' <- generate (sequence $ repeat $ choose (10, 10)) >>=
          generate . genVPropAtShare n . vPropShare
  noShprop' <- generate (sequence $ repeat $ choose (0, 10)) >>=
               generate . genVPropAtSize n .  vPropNoShare
  let prop = bimap show show prop'
      noShprop = bimap show show noShprop'

  writeDesc "Shared" metrics prop descfp
  writeDesc "Unique" metrics noShprop descfp

  -- | run brute force solve
  -- time "defConf/Shared/BForce" metrics timefp $! runBF defConf prop
  -- time "defConf/Unique/BForce" metrics timefp $! runBF defConf noShprop

  -- -- | run incremental solve
  -- time "defConf/Shared/VSMTSolve" metrics timefp $! satWith defConf prop
  -- time "allOpts/Shared/VSMTSolve" metrics timefp $! satWith allOptsConf prop

  -- -- | run and decomp
  time "defConf/Shared/ChcDecomp" metrics timefp $! runAD defConf prop
  time "allOpts/Shared/ChcDecomp" metrics timefp $! runAD allOptsConf prop

time :: NFData a => Text -> RunMetric -> FilePath -> IO a -> IO ()
time !desc !metrics@(rn, n) timefp !a = do
  start <- getCPUTime
  v <- a
  end' <- timeout 300000000 (v `seq` getCPUTime)
  let end = maybe (300 * 10^12) id end'
      diff = (fromIntegral (end - start)) / (10 ^ 12)
  print $ "Run: " ++ show rn ++ " Scale: " ++ show n ++ " TC: " ++ (unpack desc) ++ "Time: " ++ show diff
  writeTime desc metrics diff timefp
