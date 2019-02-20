import           Control.Arrow           (first, second)
import           Criterion.Main
import           Criterion.Main.Options
import           Criterion.Types         (Config (..))
import           Data.Aeson              (decodeStrict)
import           Data.Bifunctor          (bimap)
import qualified Data.ByteString         as BS (readFile)
import           Data.Either             (lefts, rights)
import           Data.List               (sort)
import           Data.Map                (size)
import qualified Data.SBV                as S (sat)
import           Data.Text               (pack, unpack)
import qualified Data.Text.IO            as T (writeFile)
import           System.IO
import           Text.Megaparsec         (parse)

import           Api
import           CaseStudy.Auto.Auto
import           CaseStudy.Auto.Parser   (langParser)
import           CaseStudy.Auto.Run
import           Config                  (defConf, emptyConf)
import           Opts
import           Run                     (runAD, runBF)
import           Result
import           Utils
import           VProp.Core
import           VProp.SBV               (toPredicate)
import           VProp.Types

-- | a large dataset of queries
-- autoFile :: FilePath
-- autoFile = "bench/AutoBench/Automotive02_merged_evolution_history_integer.json"

autoFileBool :: FilePath
autoFileBool = "bench/AutoBench/Automotive02_merged_evolution_history_boolean.json"

-- | a different file that represents a possible json
smAutoFile :: FilePath
smAutoFile = "bench/AutoBench/vsat_small_example.json"

-- | a chunk of the large autoFile above
chAutoFile :: FilePath
chAutoFile = "bench/AutoBench/vsat_small_chunk.json"

-- main :: IO (V String (Maybe ThmResult))

critConfig = defaultConfig {resamples = 2}

-- run with stack bench --profile vsat:auto --benchmark-arguments='+RTS -S -RTS --output timings.html'
main = do
  -- readfile is strict
  sJsn <- BS.readFile smAutoFile
  bJsn <- BS.readFile autoFileBool
  let (Just sAuto) = decodeStrict sJsn :: Maybe Auto
  let (Just bAuto) = decodeStrict bJsn :: Maybe Auto
      !sCs = constraints sAuto -- looks like 4298/4299 are the culprits
      !bCs = constraints bAuto
      sPs' = parse langParser "" <$> sCs
      sPs = rights sPs'

      bPs' = parse langParser "" <$> bCs
      bPs = rights bPs'

      !sProp = (naiveEncode . nestChoices . autoToVSat) $ autoAndJoin sPs
      !bProp = (naiveEncode . nestChoices . autoToVSat) $ autoAndJoin (take 350 bPs)

  res <- satWith emptyConf $! bProp
  -- res' <- runIncrementalSolve bPs
  T.writeFile "testoutputSAT" (pack . show $ res)
  -- T.writeFile "testoutputInc" (pack . show $ res')
  -- print res
  -- defaultMainWith critConfig
  --   [
  --   bgroup "vsat" [ -- bench "small file" . nfIO $ satWith emptyConf sProp
  --                 bench "large file" . nfIO $ satWith emptyConf bProp
  --                 ]
  --   ]
