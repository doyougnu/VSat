module Main where

import           Control.Arrow           (first, second)
import           Gauge.Main
import           Data.Aeson              (decodeStrict, encodeFile)
import           Control.Monad           (replicateM, foldM, liftM2)
import           Data.Bifunctor          (bimap)
import           Data.Bitraversable      (bimapM)
import qualified Data.ByteString         as BS (readFile)
import           Data.Either             (lefts, rights)
import           Data.Foldable           (foldr')
import           Data.List               (sort,splitAt,intersperse,foldl1',delete)
import           Data.Map                (size, Map, toList)
import qualified Data.Set                as Set
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

dataFile :: FilePath
dataFile = "bench/Financial/financial_merged.json"

        -- d == "D_16" = "D_1"
        -- d == "D_12" = "D_17"
        -- d == "D_6" = "D_13"
        -- d == "D_2" = "D_7"
        -- d == "D_10" = "D_3"
        -- d == "D_4" = "D_11"
        -- d == "D_8" = "D_5"
        -- d == "D_14" = "D_9"

sliceAndNegate n xs = fromList (&&&) $ bnot <$> drop n xs

ds :: [ReadableProp Text]
ds = bRef <$> ["D_0", "D_1", "D_17", "D_13", "D_7", "D_3", "D_11", "D_5", "D_9", "D_15"]

[d0, d1, d17, d13, d7, d3, d11, d5, d9, d15] = ds

mkCascadeConf n xs = conjoin $ (take n xs) ++ (bnot <$> drop n xs)

mkMultConf :: Int -> [ReadableProp d] -> ReadableProp d
mkMultConf n xs = conjoin (bnot <$> drop n xs)

justD01Conf        = mkMultConf 2 ds
justD012Conf       = mkMultConf 3 ds
justD0123Conf      = mkMultConf 4 ds
justD01234Conf     = mkMultConf 5 ds
justD012345Conf    = mkMultConf 6 ds
justD0123456Conf   = mkMultConf 7 ds
justD01234567Conf  = mkMultConf 8 ds
justD012345678Conf = mkMultConf 9 ds

pairs = mkPairs ds

[pD01Conf, pD12Conf, pD23Conf, pD34Conf, pD45Conf, pD56Conf, pD67Conf, pD78Conf, pD89Conf] = mkCompRatioPairs ds pairs

-- ((<,0), = "D_0"})
-- ((<,1), = "D_16"})
-- ((<,2), = "D_12"})
-- ((<,3), = "D_6"})
-- ((<,4), = "D_2"})
-- ((<,5), = "D_10"})
-- ((<,6), = "D_4"})
-- ((<,7), = "D_8"})
-- ((<,8), = "D_14"})
-- ((≤,0), = "D_1"})
-- ((≤,1), = "D_17"})
-- ((≤,2), = "D_13"})
-- ((≤,3), = "D_7"})
-- ((≤,4), = "D_3"})
-- ((≤,5), = "D_11"})
-- ((≤,6), = "D_5"})
-- ((≤,7), = "D_9"})
-- ((≤,8), = "D_15"})

-- dimConf' :: VProp Text String String
-- encoding for 6 configs that make sure the inequalities encompass each other
evoAwareConf = disjoin confs

deadCore = bRef "DeadCore"
mkConf x xs = (deadCore ||| x) &&& x &&& (conjoin $ bnot <$> (delete x xs))

confs = fmap (flip mkConf ds) ds

[d0Conf, d1Conf, d2Conf, d3Conf, d4Conf, d5Conf, d6Conf, d7Conf, d8Conf, d9Conf] = confs

singleVersionConf = disjoin confs

-- run with stack bench --profile vsat:auto --benchmark-arguments='+RTS -S -RTS --output timings.html'
main = do
  -- readfile is strict
  bJsn <- BS.readFile dataFile
  let (Just bAuto) = decodeStrict bJsn :: Maybe Auto
      !bCs = constraints bAuto

      bPs' = parse langParser "" <$> bCs
      bPs = rights bPs'

      -- | Hardcoding equivalencies in generated dimensions to reduce number of
      -- dimensions to 4
      sameDims :: Text -> Text
      sameDims d
        | d == "D_16" = "D_1"
        | d == "D_12" = "D_17"
        | d == "D_6" = "D_13"
        | d == "D_2" = "D_7"
        | d == "D_10" = "D_3"
        | d == "D_4" = "D_11"
        | d == "D_8" = "D_5"
        | d == "D_14" = "D_9"
        | otherwise = d

      -- !bProp = ((renameDims sameDims) . naiveEncode . autoToVSat) $ autoAndJoin bPs
      bProp' :: ReadableProp Text
      !bProp' = (renameDims sameDims . naiveEncode . autoToVSat) $ autoAndJoin bPs
      features = Set.toList $ vars bProp'
      -- bPropLength = Set.size features

  -- select a random feature to check for dead
  let
    -- (featIndex,_) = randomR (0, bPropLength) (mkStdGen 1111)
    -- deadFeature   = Set.elemAt featIndex features
  -- construct the dead feature proposition
    -- bProp         = bProp' &&& bChc "DeadCore" (bRef deadFeature) (bnot $ bRef deadFeature)
    bProp         = bProp' &&& bChc "DeadCore" (conjoin $ fmap bRef features) (conjoin $ fmap (bnot . bRef) features)

    benches :: ReadableSMTConf Text -> [Benchmark]
    benches solverConf =
      [
        mkBench' "v-->v" "V1*V2*V3*V4*V5*V6*V7*V8*V9*V10" (satWithConf (toDimProp singleVersionConf) solverConf) bProp
      -- p - v
      , mkBench' "p-->v" "V1*V2*V3*V4*V5*V6*V7*V8*V9*V10" (pOnVWithConf (toDimProp singleVersionConf) solverConf) bProp
      -- p - p
      , mkBench' "p-->p" "V1*V2*V3*V4*V5*V6*V7*V8*V9*V10" (bfWithConf (toDimProp singleVersionConf) solverConf) bProp
      -- v - p
      , mkBench' "v-->p" "V1*V2*V3*V4*V5*V6*V7*V8*V9*V10" (vOnPWithConf (toDimProp singleVersionConf) solverConf) bProp
      ]

  defaultMain
    [ -- bgroup "ABC" (benches abcDefConf)
    --   bgroup "Yices" (benches yicesDefConf)
    -- , bgroup "CVC4" (benches cvc4DefConf)
        -- bgroup "Z3" (compRatioBenches z3DefConf)
        bgroup "Z3" (benches z3DefConf)
    -- , bgroup "Boolector" (benches boolectorDefConf)
    ]

  (satWithConf (toDimProp singleVersionConf) z3DefConf) bProp >>= encodeFile "data/fin_vmodel_dead_core.json"
