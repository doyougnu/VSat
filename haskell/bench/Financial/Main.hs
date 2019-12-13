module Main where

import           Control.Arrow           (first, second)
import           Criterion.Main
import           Criterion.Main.Options
import           Criterion.Types         (Config (..))
import           Data.Aeson              (decodeStrict)
import           Control.Monad           (replicateM, foldM, liftM2)
import           Data.Bifunctor          (bimap)
import           Data.Bitraversable      (bimapM)
import qualified Data.ByteString         as BS (readFile)
import           Data.Either             (lefts, rights)
import           Data.Foldable           (foldr')
import           Data.List               (sort,splitAt,intersperse,foldl1',delete)
import           Data.Map                (size, Map, toList)
import qualified Data.SBV                as S
import qualified Data.SBV.Control        as SC
import qualified Data.SBV.Internals      as SI
import           Data.Text               (pack, unpack,Text)
import qualified Data.Text.IO            as T (writeFile)
import           System.IO
import           Text.Megaparsec         (parse)

import           Api
import           CaseStudy.Auto.Auto
import           CaseStudy.Auto.Parser   (langParser)
import           CaseStudy.Auto.Run
import           CaseStudy.Auto.CompactEncode
import           Config
import           Opts
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

mkConf x xs = x &&& (conjoin $ bnot <$> (delete x xs))

confs = fmap (flip mkConf ds) ds

[d0Conf, d1Conf, d2Conf, d3Conf, d4Conf, d5Conf, d6Conf, d7Conf, d8Conf, d9Conf] = confs
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
      !bProp = ((renameDims sameDims) . naiveEncode . autoToVSat) $ autoAndJoin bPs
      dmapping = getDimMap $ autoAndJoin (bPs)
      !bPropOpts = applyOpts defConf bProp

  -- | convert choice preserving fmfs to actual confs
  -- [justV1]         <- genConfigPool justD0Conf
  -- [justV2]         <- genConfigPool justD1Conf
  -- [justV3]         <- genConfigPool justD2Conf
  -- [justV4]         <- genConfigPool justD3Conf
  [justV12']        <- genConfigPool justD01Conf
  [justV123]       <- genConfigPool justD012Conf
  [justV1234]      <- genConfigPool justD0123Conf
  [justV12345]     <- genConfigPool justD01234Conf
  [justV123456]    <- genConfigPool justD012345Conf
  [justV1234567]   <- genConfigPool justD0123456Conf
  [justV12345678]  <- genConfigPool justD01234567Conf
  [justV123456789] <- genConfigPool justD012345678Conf
  -- [ppVAll]       <- genConfigPool d0123456789Conf

  [ppV1]  <- genConfigPool d0Conf
  [ppV2]  <- genConfigPool d1Conf
  [ppV3]  <- genConfigPool d2Conf
  [ppV4]  <- genConfigPool d3Conf
  [ppV5]  <- genConfigPool d4Conf
  [ppV6]  <- genConfigPool d5Conf
  [ppV7]  <- genConfigPool d6Conf
  [ppV8]  <- genConfigPool d7Conf
  [ppV9]  <- genConfigPool d8Conf
  [ppV10] <- genConfigPool d9Conf

  -- compression ratio pairs
  justV12  <- genConfigPool pD01Conf
  justV23  <- genConfigPool pD12Conf
  justV34  <- genConfigPool pD23Conf
  justV45  <- genConfigPool pD34Conf
  justV56  <- genConfigPool pD45Conf
  justV67  <- genConfigPool pD56Conf
  justV78  <- genConfigPool pD67Conf
  justV89  <- genConfigPool pD78Conf
  justV910 <- genConfigPool pD89Conf

  -- | Compression Ratio Confs to select out other dimensions
  [v01Conf,v12Conf,v23Conf,v34Conf,v45Conf,v56Conf,v67Conf,v78Conf,v89Conf] <- mkCompRatioConfs ds pairs

  let
    -- | choice preserving props
    -- (Just justbPropV1)         = selectVariant justV1 bProp
    -- (Just justbPropV2)         = selectVariant justV2 bProp
    -- (Just justbPropV3)         = selectVariant justV3 bProp
    -- (Just justbPropV4)         = selectVariant justV4 bProp
    justbPropV12        = selectVariant justV12' bProp
    justbPropV123       = selectVariant justV123 bProp
    justbPropV1234      = selectVariant justV1234 bProp
    justbPropV12345     = selectVariant justV12345 bProp
    justbPropV123456    = selectVariant justV123456 bProp
    justbPropV1234567   = selectVariant justV1234567 bProp
    justbPropV12345678  = selectVariant justV12345678 bProp
    justbPropV123456789 = selectVariant justV123456789 bProp

    -- | single version props
    !bPropV1  = selectVariantTotal ppV1  bProp
    !bPropV2  = selectVariantTotal ppV2  bProp
    !bPropV3  = selectVariantTotal ppV3  bProp
    !bPropV4  = selectVariantTotal ppV4  bProp
    !bPropV5  = selectVariantTotal ppV5  bProp
    !bPropV6  = selectVariantTotal ppV6  bProp
    !bPropV7  = selectVariantTotal ppV7  bProp
    !bPropV8  = selectVariantTotal ppV8  bProp
    !bPropV9  = selectVariantTotal ppV9  bProp
    !bPropV10 = selectVariantTotal ppV10 bProp

    benches :: ReadableSMTConf Text -> [Benchmark]
    benches solverConf =
      [ mkBench "v-->v" "V1"  d0Conf (satWithConf (toDimProp d0Conf) solverConf) bProp
      , mkBench "v-->v" "V2"  d1Conf (satWithConf (toDimProp d1Conf) solverConf) bProp
      , mkBench "v-->v" "V3"  d2Conf (satWithConf (toDimProp d2Conf) solverConf) bProp
      , mkBench "v-->v" "V4"  d3Conf (satWithConf (toDimProp d3Conf) solverConf) bProp
      , mkBench "v-->v" "V5"  d4Conf (satWithConf (toDimProp d4Conf) solverConf) bProp
      , mkBench "v-->v" "V6"  d5Conf (satWithConf (toDimProp d5Conf) solverConf) bProp
      , mkBench "v-->v" "V7"  d6Conf (satWithConf (toDimProp d6Conf) solverConf) bProp
      , mkBench "v-->v" "V8"  d7Conf (satWithConf (toDimProp d7Conf) solverConf) bProp
      , mkBench "v-->v" "V9"  d8Conf (satWithConf (toDimProp d8Conf) solverConf) bProp
      , mkBench "v-->v" "V10" d9Conf (satWithConf (toDimProp d9Conf) solverConf) bProp
      , mkBench' "v-->v" "EvolutionAware" (satWithConf (toDimProp evoAwareConf) solverConf) bProp

      , mkBench "v-->v" "V1*V2"                          justD01Conf (satWith solverConf) justbPropV12
      , mkBench "v-->v" "V1*V2*V3"                       justD012Conf (satWith solverConf) justbPropV123
      , mkBench "v-->v" "V1*V2*V3*V4"                    justD0123Conf (satWith solverConf) justbPropV1234
      , mkBench "v-->v" "V1*V2*V3*V4*V5"                 justD01234Conf (satWith solverConf) justbPropV12345
      , mkBench "v-->v" "V1*V2*V3*V4*V5*V6"              justD012345Conf (satWith solverConf) justbPropV123456
      , mkBench "v-->v" "V1*V2*V3*V4*V5*V6*V7"           justD0123456Conf (satWith solverConf) justbPropV1234567
      , mkBench "v-->v" "V1*V2*V3*V4*V5*V6*V7*V8"        justD01234567Conf (satWith solverConf) justbPropV12345678
      , mkBench "v-->v" "V1*V2*V3*V4*V5*V6*V7*V8*V9"     justD012345678Conf (satWith solverConf) justbPropV123456789
      , mkBench' "v-->v" "V1*V2*V3*V4*V5*V6*V7*V8*V9*V10" (satWith solverConf) bProp
  -- p - v
      , mkBench "p-->v" "V1" d0Conf (pOnV solverConf) bPropV1
      , mkBench "p-->v" "V2" d1Conf (pOnV solverConf) bPropV2
      , mkBench "p-->v" "V3" d2Conf (pOnV solverConf) bPropV3
      , mkBench "p-->v" "V4" d3Conf (pOnV solverConf) bPropV4
      , mkBench "p-->v" "V5" d4Conf (pOnV solverConf) bPropV5
      , mkBench "p-->v" "V6" d5Conf (pOnV solverConf) bPropV6
      , mkBench "p-->v" "V7" d6Conf (pOnV solverConf) bPropV7
      , mkBench "p-->v" "V8" d7Conf (pOnV solverConf) bPropV8
      , mkBench "p-->v" "V9" d8Conf (pOnV solverConf) bPropV9
      , mkBench "p-->v" "V10" d9Conf  (pOnV solverConf) bPropV10
      , mkBench' "p-->v" "EvolutionAware" (pOnVWithConf (toDimProp evoAwareConf) solverConf) bProp
      , mkBench "p-->v" "V1*V2"                          justD01Conf (pOnV solverConf) justbPropV12
      , mkBench "p-->v" "V1*V2*V3"                       justD012Conf (pOnV solverConf) justbPropV123
      , mkBench "p-->v" "V1*V2*V3*V4"                    justD0123Conf (pOnV solverConf) justbPropV1234
      , mkBench "p-->v" "V1*V2*V3*V4*V5"                 justD01234Conf (pOnV solverConf) justbPropV12345
      , mkBench "p-->v" "V1*V2*V3*V4*V5*V6"              justD012345Conf (pOnV solverConf) justbPropV123456
      , mkBench "p-->v" "V1*V2*V3*V4*V5*V6*V7"           justD0123456Conf (pOnV solverConf) justbPropV1234567
      , mkBench "p-->v" "V1*V2*V3*V4*V5*V6*V7*V8"        justD01234567Conf (pOnV solverConf) justbPropV12345678
      , mkBench "p-->v" "V1*V2*V3*V4*V5*V6*V7*V8*V9"     justD012345678Conf (pOnV solverConf) justbPropV123456789
      , mkBench' "p-->v" "V1*V2*V3*V4*V5*V6*V7*V8*V9*V10" (pOnV solverConf) bProp
  -- p - p
      , mkBench "p-->p" "V1" d0Conf (bfWith  solverConf) bPropV1
      , mkBench "p-->p" "V2" d1Conf (bfWith  solverConf) bPropV2
      , mkBench "p-->p" "V3" d2Conf (bfWith  solverConf) bPropV3
      , mkBench "p-->p" "V4" d3Conf (bfWith  solverConf) bPropV4
      , mkBench "p-->p" "V5" d4Conf (bfWith  solverConf) bPropV5
      , mkBench "p-->p" "V6" d5Conf (bfWith  solverConf) bPropV6
      , mkBench "p-->p" "V7" d6Conf (bfWith  solverConf) bPropV7
      , mkBench "p-->p" "V8" d7Conf (bfWith  solverConf) bPropV8
      , mkBench "p-->p" "V9" d8Conf (bfWith  solverConf) bPropV9
      , mkBench "p-->p" "V10"d9Conf  (bfWith  solverConf) bPropV10
      , mkBench' "p-->p" "EvolutionAware" (bfWithConf (toDimProp evoAwareConf) solverConf) bProp
      , mkBench "p-->p" "V1*V2"                          justD01Conf (bfWith solverConf) justbPropV12
      , mkBench "p-->p" "V1*V2*V3"                       justD012Conf (bfWith solverConf) justbPropV123
      , mkBench "p-->p" "V1*V2*V3*V4"                    justD0123Conf (bfWith solverConf) justbPropV1234
      , mkBench "p-->p" "V1*V2*V3*V4*V5"                 justD01234Conf (bfWith solverConf) justbPropV12345
      , mkBench "p-->p" "V1*V2*V3*V4*V5*V6"              justD012345Conf (bfWith solverConf) justbPropV123456
      , mkBench "p-->p" "V1*V2*V3*V4*V5*V6*V7"           justD0123456Conf (bfWith solverConf) justbPropV1234567
      , mkBench "p-->p" "V1*V2*V3*V4*V5*V6*V7*V8"        justD01234567Conf (bfWith solverConf) justbPropV12345678
      , mkBench "p-->p" "V1*V2*V3*V4*V5*V6*V7*V8*V9"     justD012345678Conf (bfWith solverConf) justbPropV123456789
      , mkBench' "p-->p" "V1*V2*V3*V4*V5*V6*V7*V8*V9*V10" (bfWith solverConf) bProp
 -- v - p
      , mkBench "v-->p" "V1"  d0Conf (vOnPWithConf (toDimProp d0Conf) solverConf) bProp
      , mkBench "v-->p" "V2"  d1Conf (vOnPWithConf (toDimProp d1Conf) solverConf) bProp
      , mkBench "v-->p" "V3"  d2Conf (vOnPWithConf (toDimProp d2Conf) solverConf) bProp
      , mkBench "v-->p" "V4"  d3Conf (vOnPWithConf (toDimProp d3Conf) solverConf) bProp
      , mkBench "v-->p" "V5"  d4Conf (vOnPWithConf (toDimProp d4Conf) solverConf) bProp
      , mkBench "v-->p" "V6"  d5Conf (vOnPWithConf (toDimProp d5Conf) solverConf) bProp
      , mkBench "v-->p" "V7"  d6Conf (vOnPWithConf (toDimProp d6Conf) solverConf) bProp
      , mkBench "v-->p" "V8"  d7Conf (vOnPWithConf (toDimProp d7Conf) solverConf) bProp
      , mkBench "v-->p" "V9"  d8Conf (vOnPWithConf (toDimProp d8Conf) solverConf) bProp
      , mkBench "v-->p" "V10" d9Conf (vOnPWithConf (toDimProp d9Conf) solverConf) bProp
      , mkBench' "v-->p" "EvolutionAware" (vOnPWithConf (toDimProp evoAwareConf) solverConf) bProp

      , mkBench "v-->p" "V1*V2"                          justD01Conf (vOnPWith solverConf) justbPropV12
      , mkBench "v-->p" "V1*V2*V3"                       justD012Conf (vOnPWith solverConf) justbPropV123
      , mkBench "v-->p" "V1*V2*V3*V4"                    justD0123Conf (vOnPWith solverConf) justbPropV1234
      , mkBench "v-->p" "V1*V2*V3*V4*V5"                 justD01234Conf (vOnPWith solverConf) justbPropV12345
      , mkBench "v-->p" "V1*V2*V3*V4*V5*V6"              justD012345Conf (vOnPWith solverConf) justbPropV123456
      , mkBench "v-->p" "V1*V2*V3*V4*V5*V6*V7"           justD0123456Conf (vOnPWith solverConf) justbPropV1234567
      , mkBench "v-->p" "V1*V2*V3*V4*V5*V6*V7*V8"        justD01234567Conf (vOnPWith solverConf) justbPropV12345678
      , mkBench "v-->p" "V1*V2*V3*V4*V5*V6*V7*V8*V9"     justD012345678Conf (vOnPWith solverConf) justbPropV123456789
      , mkBench' "v-->p" "V1*V2*V3*V4*V5*V6*V7*V8*V9*V10" (vOnPWith solverConf) bProp
      ]


    -- | Compression Ratio props, we start counting by 1 here
    justbPropV12'  = selectVariant v01Conf bProp
    justbPropV23   = selectVariant v12Conf bProp
    !justbPropV34  = selectVariant v23Conf bProp
    !justbPropV45  = selectVariant v34Conf bProp
    !justbPropV56  = selectVariant v45Conf bProp
    !justbPropV67  = selectVariant v56Conf bProp
    !justbPropV78  = selectVariant v67Conf bProp
    !justbPropV89  = selectVariant v78Conf bProp
    !justbPropV910 = selectVariant v89Conf bProp

    -- use mkBench' because we are restricting the solver with an fmf not
    -- through pre-done selection
    compRatioBenches :: ReadableSMTConf Text -> [Benchmark]
    compRatioBenches solverConf =
      [
        -- v --> v
        mkCompBench "v-->v" "V1*V2"  (satWithConf (toDimProp pD01Conf) solverConf) justbPropV12'
      , mkCompBench "v-->v" "V2*V3"  (satWithConf (toDimProp pD12Conf) solverConf) justbPropV23
      , mkCompBench "v-->v" "V3*V4"  (satWithConf (toDimProp pD23Conf) solverConf) justbPropV34
      , mkCompBench "v-->v" "V4*V5"  (satWithConf (toDimProp pD34Conf) solverConf) justbPropV45
      , mkCompBench "v-->v" "V5*V6"  (satWithConf (toDimProp pD45Conf) solverConf) justbPropV56
      , mkCompBench "v-->v" "V6*V7"  (satWithConf (toDimProp pD56Conf) solverConf) justbPropV67
      , mkCompBench "v-->v" "V7*V8"  (satWithConf (toDimProp pD67Conf) solverConf) justbPropV78
      , mkCompBench "v-->v" "V8*V9"  (satWithConf (toDimProp pD78Conf) solverConf) justbPropV89
      , mkCompBench "v-->v" "V9*V10"  (satWithConf (toDimProp pD89Conf) solverConf) justbPropV910

        -- p --> v
      , mkCompBench "p-->v" "V1*V2"  (pOnVWithConf (toDimProp pD01Conf) solverConf) justbPropV12'
      , mkCompBench "p-->v" "V2*V3"  (pOnVWithConf (toDimProp pD12Conf) solverConf) justbPropV23
      , mkCompBench "p-->v" "V3*V4"  (pOnVWithConf (toDimProp pD23Conf) solverConf) justbPropV34
      , mkCompBench "p-->v" "V4*V5"  (pOnVWithConf (toDimProp pD34Conf) solverConf) justbPropV45
      , mkCompBench "p-->v" "V5*V6"  (pOnVWithConf (toDimProp pD45Conf) solverConf) justbPropV56
      , mkCompBench "p-->v" "V6*V7"  (pOnVWithConf (toDimProp pD56Conf) solverConf) justbPropV67
      , mkCompBench "p-->v" "V7*V8"  (pOnVWithConf (toDimProp pD67Conf) solverConf) justbPropV78
      , mkCompBench "p-->v" "V8*V9"  (pOnVWithConf (toDimProp pD78Conf) solverConf) justbPropV89
      , mkCompBench "p-->v" "V9*V10" (pOnVWithConf (toDimProp pD89Conf) solverConf) justbPropV910

        -- p --> p
      , mkCompBench "p-->p" "V1*V2"  (bfWithConf (toDimProp pD01Conf) solverConf) justbPropV12'
      , mkCompBench "p-->p" "V2*V3"  (bfWithConf (toDimProp pD12Conf) solverConf) justbPropV23
      , mkCompBench "p-->p" "V3*V4"  (bfWithConf (toDimProp pD23Conf) solverConf) justbPropV34
      , mkCompBench "p-->p" "V4*V5"  (bfWithConf (toDimProp pD34Conf) solverConf) justbPropV45
      , mkCompBench "p-->p" "V5*V6"  (bfWithConf (toDimProp pD45Conf) solverConf) justbPropV56
      , mkCompBench "p-->p" "V6*V7"  (bfWithConf (toDimProp pD56Conf) solverConf) justbPropV67
      , mkCompBench "p-->p" "V7*V8"  (bfWithConf (toDimProp pD67Conf) solverConf) justbPropV78
      , mkCompBench "p-->p" "V8*V9"  (bfWithConf (toDimProp pD78Conf) solverConf) justbPropV89
      , mkCompBench "p-->p" "V9*V10" (bfWithConf (toDimProp pD89Conf) solverConf) justbPropV910

        -- p --> v
      , mkCompBench "p-->v" "V1*V2"  (vOnPWithConf (toDimProp pD01Conf) solverConf) justbPropV12'
      , mkCompBench "p-->v" "V2*V3"  (vOnPWithConf (toDimProp pD12Conf) solverConf) justbPropV23
      , mkCompBench "p-->v" "V3*V4"  (vOnPWithConf (toDimProp pD23Conf) solverConf) justbPropV34
      , mkCompBench "p-->v" "V4*V5"  (vOnPWithConf (toDimProp pD34Conf) solverConf) justbPropV45
      , mkCompBench "p-->v" "V5*V6"  (vOnPWithConf (toDimProp pD45Conf) solverConf) justbPropV56
      , mkCompBench "p-->v" "V6*V7"  (vOnPWithConf (toDimProp pD56Conf) solverConf) justbPropV67
      , mkCompBench "p-->v" "V7*V8"  (vOnPWithConf (toDimProp pD67Conf) solverConf) justbPropV78
      , mkCompBench "p-->v" "V8*V9"  (vOnPWithConf (toDimProp pD78Conf) solverConf) justbPropV89
      , mkCompBench "p-->v" "V9*V10" (vOnPWithConf (toDimProp pD89Conf) solverConf) justbPropV910
      ]

  defaultMain
    [ -- bgroup "ABC" (benches abcDefConf)
    --   bgroup "Yices" (benches yicesDefConf)
    -- , bgroup "CVC4" (benches cvc4DefConf)
      bgroup "Z3" (benches z3DefConf)
    , bgroup "Z3OnlySat" (benches z3DefConfOnlySat)
        -- bgroup "Z3" (compRatioBenches z3DefConf)
    -- , bgroup "Boolector" (benches boolectorDefConf)
    ]

  -- putStrLn $ show $ ds
  -- putStrLn $ show $ pairs
  -- ts <- (mkCompRatioConfs ds pairs :: IO [VProp.Types.Config Text])
  -- mapM_ (putStrLn . show) ts
