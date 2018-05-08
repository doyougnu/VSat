module Main where

import Criterion.Main as C

import Run
import Criterion.Types
import Data.Csv
import Data.Text (Text,pack)
import qualified Data.Vector as V
import Prelude hiding (writeFile, appendFile)
import GHC.Generics (Generic)
import qualified Data.ByteString.Char8 as BS (pack)
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
  mapM_ benchAndInc $ zip [1..] $ [100,200..3000] >>= replicate 2

-- | The run number, used to join descriptor and timing data later
type RunNum = Integer
type TermSize = Integer

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

writeDesc :: String -> (RunNum, TermSize) -> VProp Readable -> IO ()
writeDesc desc (rn, n) prop' = do
  let descriptorsFs = [ numTerms
                      , numChc
                      , numPlain
                      , numSharedDims
                      , numSharedPlain
                      , maxShared
                      ]
      prop = toReadable prop'
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

toReadable :: VProp Readable -> VProp String
toReadable = fmap readStr


benchAndInc :: (RunNum, Integer) -> IO ()
benchAndInc metrics@(rn, n) = do
  noShProp <- generate $ mkLargeVProp (fromInteger n) vPropNoShare :: IO (VProp Readable)
  prop <- generate $ mkLargeVProp (fromInteger n) arbitrary :: IO (VProp Readable)
  writeDesc "Unique" metrics noShProp
  writeDesc "Shared" metrics prop

  (res, _, _) <- runEnv True True False [] $ toReadable noShProp

  print $ "Run: " ++ show rn ++ " Scale: " ++ show n ++ " | " ++ case res of
    R Nothing -> "Nothing"
    R (Just _) -> "got model"
    L xs       -> show $ length xs
    Vr xs      -> show $ length xs

  -- C.defaultMainWith myConfig
  --   [ C.bgroup ("Unique/" ++ show rn ++ "/" ++ show n)
  --     [ bench "Variational Solve" $ C.nfIO (runEnv False False False [] noShProp)
  --     , bench "And Decomposition" $ C.nfIO (runEnv True True False [] noShProp)
  --     ]
  --   , C.bgroup ("Shared/" ++ show rn ++ "/" ++ show n)
  --     [ bench "Variational Solve" $ C.nfIO (runEnv False False False [] prop)
  --     , bench "And Decomposition" $ C.nfIO (runEnv True True False [] prop)
  --     ]
  --   ]
