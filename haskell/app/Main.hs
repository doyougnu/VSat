module Main where

import Criterion.Main as C

import Run
import Criterion.Types
import Data.Csv
import Data.Text (Text,pack, unpack)
import qualified Data.Vector as V
import Prelude hiding (writeFile, appendFile)
import GHC.Generics (Generic)
import qualified Data.ByteString.Char8 as BS (pack)
import Data.ByteString.Lazy (writeFile, appendFile)
import VProp ( VProp
             , Readable
             , readStr
             , vPropNoShare
             , genVPropAtSize
             , vPropShare
             , numTerms
             , numChc
             , numPlain
             , numSharedDims
             , numSharedPlain
             , maxShared
             )
import Test.QuickCheck (generate, arbitrary, choose)

import System.CPUTime
-- import
-- import Data.Time.Clock
-- import Data.Time.Calendar

myConfig :: Config
myConfig = C.defaultConfig { resamples = 2 }

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
  mapM_ benchRandomSample $ zip [1..] $ [10,20..2000] >>= replicate 100

-- | The run number, used to join descriptor and timing data later
type RunNum = Int

-- | The Term size used to generate an arbitrary VProp of size TermSize
type TermSize = Int

type RunMetric = (RunNum, TermSize)

-- | Give a descriptor, run metrics, and a prop, generate the descriptor metrics
-- for the prop and write them out to a csv
writeDesc :: Show a => String -> RunMetric -> VProp a -> IO ()
writeDesc desc (rn, n) prop' = do
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

writeTime :: Text -> RunMetric -> Double -> IO ()
writeTime str (rn, n) time_ = appendFile timingFile $ encodeByName headers $ pure row
  where row = TimeData str rn n time_
        headers = V.fromList $ BS.pack <$> ["name__", "runNum__", "scale__", "time__"]

-- -- | Bench only and decomposition and Incremental Solve given run metrics to
-- -- generate the prop with and log the run
-- benchAndInc :: RunMetric -> IO ()
-- benchAndInc metrics@(rn, n) = do
--   noShProp <- generate $ genVPropAtSize (fromInteger n) vPropNoShare :: IO (VProp Readable)
--   prop <- generate $ genVPropAtSize (fromInteger n) arbitrary :: IO (VProp Readable)
--   writeDesc "Unique" metrics noShProp
--   writeDesc "Shared" metrics prop

--   -- | run incremental solve
--   (tm1, _) <- time $! runEnv False False False [] (toReadable noShProp)
--   (tm2, _) <- time $! runEnv False False False [] (toReadable prop)

--   -- | run and decomp
--   (tm3, _) <- time $! runEnv True True False [] (toReadable noShProp)
--   (tm4, _) <- time $! runEnv True True False [] (toReadable prop)

--   -- | log the times
--   writeTime "Unique/VSolve" metrics tm1
--   writeTime "Shared/VSolve" metrics tm2
--   writeTime "Unique/AndDecomp" metrics tm3
--   writeTime "Shared/AndDecomp" metrics tm4

--   print $ "Run: " ++ show rn ++ " Scale: " ++ show n ++ " | " ++ " Times: " ++
--     "VSolve: " ++ show tm1 ++ " | " ++ show tm2 ++ " | " ++ "AndDecomp: " ++ show tm3 ++ " | " ++ show tm4

-- | Given run metrics, benchmark a data where the frequency of terms is randomly distributed from 0 to 10, dimensions and variables are sampled from a bound pool so sharing is also very possible.
benchRandomSample :: RunMetric -> IO ()
benchRandomSample metrics@(_, n) = do
  prop' <- generate (sequence $ repeat $ choose (0, 10)) >>=
          generate . genVPropAtSize n .  vPropShare
  let prop = fmap show prop'

  writeDesc "Shared" metrics prop

  -- | run incremental solve
  time "Shared/VSolve" metrics $! runEnv False False False [] prop `seq` return ()

  -- | run and decomp
  time "Shared/AndDecomp" metrics $! runEnv True True False [] prop `seq` return ()

time :: Text -> RunMetric -> IO a -> IO ()
time desc metrics@(rn, n) a = do
  start <- getCPUTime
  v <- a `seq` return ()
  end <- getCPUTime
  let diff = (fromIntegral (end - start)) / (10 ^ 12)
  print $ "Run: " ++ show rn ++ " Scale: " ++ show n ++ " TC: " ++ (unpack desc) ++ " Sol Length: "
  writeTime desc metrics diff
