module Main where

import           Control.Arrow           (first, second)
import           Gauge.Main
import           Data.Aeson              (decodeStrict, encodeFile)
import           Control.Monad           (replicateM, foldM, liftM2)
import           Data.Bifunctor          (bimap)
import           Data.Bitraversable      (bimapM)
import qualified Data.ByteString         as BS (readFile)
import qualified Data.Set                as Set
import           Data.Either             (lefts, rights)
import           Data.Foldable           (foldr')
import           Data.List               (sort,delete,intersperse)
import           Data.Map                (size, Map, (!))
import qualified Data.SBV                as S
import qualified Data.SBV.Control        as SC
import qualified Data.SBV.Internals      as SI
import           Data.Text               (pack, unpack,Text)
import qualified Data.Text.IO            as T (writeFile)
import           System.IO
import           Text.Megaparsec         (parse)
import           System.Random           (mkStdGen, randomR)

import           Api
import           CaseStudy.Auto.Auto
import           CaseStudy.Auto.Lang
import           CaseStudy.Auto.Parser   (langParser)
import           CaseStudy.Auto.Run
import           CaseStudy.Auto.CompactEncode
import           Config
import           Opts
import           Json
import           Run                     (runAD, runBF)
import           Result
import           Utils
import           VProp.Core
import           VProp.SBV               (toPredicate)
import           VProp.Types

import           Core

autoFileBool :: FilePath
autoFileBool = "bench/AutoBench/Automotive02_merged_evolution_history_boolean.json"

-- | a different file that represents a possible json
smAutoFile :: FilePath
smAutoFile = "bench/AutoBench/vsat_small_example.json"

-- | a chunk of the large autoFile above
chAutoFile :: FilePath
chAutoFile = "bench/AutoBench/vsat_small_chunk.json"

-- main :: IO (V String (Maybe ThmResult))

sliceAndNegate n xs = fromList (&&&) $ bnot <$> drop n xs

ds :: [ReadableProp Text]
ds = bRef <$> ["D_0","D_1","D_2","D_3"]
-- D_0 /\    D_2   /\     D_4   /\  D_5
-- <0 /\ <=0 /\ <1 /\ <=1 /\ <2 /\ <= 2

[d0, d2, d4, d5] = ds

-- | This is bittner's encoding, see section 2 of:
-- https://link.springer.com/chapter/10.1007%2F978-3-030-30446-1_7
confs' =  (conjoin $ (disjoin ds):[(bnot x ||| bnot y) | x <- ds, y <- ds, x /= y])
singleVersionConf' = (deadCore &&& confs') ||| (bnot deadCore &&& confs')

deadCore = bRef "DeadCore"
mkConf x xs = (deadCore ||| x) &&& x &&& (conjoin $ bnot <$> (delete x xs))

confs = fmap (flip mkConf ds) ds

[d0Conf, d1Conf, d2Conf, d3Conf, d4Conf, d5Conf, d6Conf, d7Conf, d8Conf, d9Conf] = confs

singleVersionConf = disjoin confs

-- run with stack bench --profile vsat:auto --benchmark-arguments='+RTS -S -RTS --output timings.html'
main = do
  -- readfile is strict
  bJsn <- BS.readFile autoFileBool
  let (Just bAuto) = decodeStrict bJsn :: Maybe Auto
      !bCs = constraints bAuto
      bPs' = parse langParser "" <$> bCs
      bPs = fmap (simplifyCtxs . renameCtxs sameCtxs) $ rights bPs'

      -- | Hardcoding equivalencies in generated dimensions to reduce number of
      -- dimensions to 4
      -- sameDims :: Text -> Text
      -- sameDims d
      --   | d == "D_1" = "D_2"
      --   | d == "D_3" = "D_4"
      --   | otherwise = d

      bProp' :: ReadableProp Text
      !bProp' = (naiveEncode . autoToVSat) $ autoAndJoin (bPs)
      features = Set.toList $ vars bProp'
      -- bPropLength = Set.size features

  let
    -- (featIndex,_) = randomR (0, bPropLength) (mkStdGen 1111)
    -- deadFeature   = Set.elemAt featIndex features
  -- construct the dead feature proposition
--  bProp         = bProp' &&& bChc "DeadCore" (bRef deadFeature) (bnot $ bRef deadFeature)
    bProp         = bProp' &&& bChc "DeadCore" (conjoin $ fmap bRef features) (conjoin $ fmap (bnot . bRef) features)

  -- Convert the fmf's to actual configurations
    benches :: ReadableSMTConf Text -> [Benchmark]
    benches solverConf = [
        -- v - v
          mkBench' "v-->v" "V1*V2*V3*V4"  (satWithConf (toDimProp singleVersionConf) solverConf) bProp

        -- p - v
        , mkBench' "p-->v" "V1*V2*V3*V4"  (pOnVWithConf (toDimProp singleVersionConf) solverConf) bProp

        -- p - p
        , mkBench' "p-->p" "V1*V2*V3*V4"  (bfWithConf (toDimProp singleVersionConf) solverConf) bProp

        -- v - p
        , mkBench' "v-->p" "V1*V2*V3*V4"  (vOnPWithConf (toDimProp singleVersionConf) solverConf) bProp
        ]

  defaultMain
    [  bgroup "Z3" (benches z3DefConf)
      -- bgroup "Z3" (compRatioBenches z3DefConf)
    -- , bgroup "CVC4" (benches cvc4DefConf)
    -- , bgroup "Yices" (benches yicesDefConf)
    -- , bgroup "Boolector" (benches boolectorDefConf)
    ]

  (satWithConf (toDimProp singleVersionConf) z3DefConf) bProp >>= encodeFile "../../data/auto_vmodel_dead_core.json"
