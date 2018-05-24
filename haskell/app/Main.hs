module Main where

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
import Test.QuickCheck (generate, choose)
import Control.DeepSeq (deepseq, NFData)

import System.CPUTime
import System.Environment
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

eraseFile :: FilePath -> IO ()
eraseFile = flip writeFile ""

-- run with stack build; stack bench --benchmark-arguments "timing_file desc_file"
main :: IO ()
main = do
  (timingFile:descFile:_) <- getArgs >>= traverse (prependDate . (flip (++) ".csv"))
  mapM_ eraseFile [descFile, timingFile]
  mapM_ (benchRandomSample descFile timingFile) $ zip [1..] $ [10,20..500] >>= replicate 100

-- | The run number, used to join descriptor and timing data later
type RunNum = Int

-- | The Term size used to generate an arbitrary VProp of size TermSize
type TermSize = Int

-- | The Run Number and TermSize used to generate the prop for the run
type RunMetric = (RunNum, TermSize)

-- | Give a descriptor, run metrics, and a prop, generate the descriptor metrics
-- for the prop and write them out to a csv
writeDesc :: Show a => String -> RunMetric -> VProp a -> FilePath -> IO ()
writeDesc desc (rn, n) prop' descFile = do
  let descriptorsFs = [ numTerms
                      , numChc
                      , numPlain
                      , numSharedDims
                      , numSharedPlain
                      , maxShared
                      ]
      prop = show <$> prop'
      [s,c,p,sd,sp,ms] = descriptorsFs <*> pure prop
      row = RunData (pack desc) rn n s c p sd sp ms
  appendFile descFile $ encodeByName headers $ pure row
  where
    headers = V.fromList $ BS.pack <$>
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

-- | Given a file path, get the year, date and time of the run and prepend it to
-- the filepath
prependDate :: FilePath -> IO FilePath
prependDate str = do (year, month, day) <- getCurrentTime >>= return . toGregorian . utctDay
                     return $ mconcat [show year, "-", show month, "-", show day, "-", str]


-- | Given some string, a run metric (the parameters for the run) a time and a
-- file path, perform the IO effect
writeTime :: Text -> RunMetric -> Double -> FilePath -> IO ()
writeTime str (rn, n) time_ timingFile = appendFile timingFile . encodeByName headers $ pure row
  where row = TimeData str rn n time_
        headers = V.fromList $ BS.pack <$> ["name__", "runNum__", "scale__", "time__"]

-- | Given run metrics, benchmark a data where the frequency of terms is
-- randomly distributed from 0 to 10, dimensions and variables are sampled from
-- a bound pool so sharing is also very possible.
benchRandomSample :: FilePath -> FilePath -> RunMetric -> IO ()
benchRandomSample descfp timefp metrics@(_, n) = do
  prop' <- generate (sequence $ repeat $ choose (0, 10)) >>=
          generate . genVPropAtSize n .  vPropShare
  -- noShprop' <- generate (sequence $ repeat $ choose (0, 10)) >>=
  --              generate . genVPropAtSize n .  vPropNoShare
  let prop = fmap show prop'
      -- noShprop = fmap show noShprop'

  writeDesc "Shared" metrics prop descfp
  -- writeDesc "Unique" metrics noShprop descfp

  -- | run brute force solve
  time "Shared/BForce" metrics timefp $! runEnv True False False [] prop
  -- time "Unique/BForce" metrics timefp $! runEnv True False False [] noShprop

  -- | run incremental solve
  time "Shared/VSolve" metrics timefp $! runEnv False False False [] prop
  -- time "Unique/VSolve" metrics timefp $! runEnv False False False [] noShprop

  -- | run and decomp
  time "Shared/ChcDecomp" metrics timefp $! runEnv True True False [] prop
  -- time "Unique/ChcDecomp" metrics timefp $! runEnv True True False [] noShprop

time :: NFData a => Text -> RunMetric -> FilePath -> IO a -> IO ()
time !desc !metrics@(rn, n) timefp !a = do
  start <- getCPUTime
  v <- a
  end <- v `deepseq` getCPUTime
  let diff = (fromIntegral (end - start)) / (10 ^ 12)
  print $ "Run: " ++ show rn ++ " Scale: " ++ show n ++ " TC: " ++ (unpack desc) ++ "Time: " ++ show diff
  writeTime desc metrics diff timefp