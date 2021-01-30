module Main where

import           Control.Arrow           (first, second)
import           Gauge
import           Data.Aeson              (decodeStrict, encodeFile)
import           Control.Monad           (replicateM, foldM, liftM2)
import           Data.Bifunctor          (bimap)
import           Data.Bitraversable      (bimapM)
import qualified Data.ByteString         as BS (readFile)
import           Data.Either             (lefts, rights)
import           Data.Foldable           (foldr')
import           Data.List               (sort,delete,intersperse)
import           Data.Map                (size, Map, (!))
import qualified Data.SBV                as S
import qualified Data.SBV.Control        as SC
import qualified Data.SBV.Internals      as SI
import           Data.Text               (pack, unpack,Text)
import qualified Data.Text.IO            as T (writeFile, appendFile)
import           System.IO
import           Text.Megaparsec         (parse)
import           Data.Time.Calendar
import           Data.Time

import           Api
import           CaseStudy.Auto.Auto
import           CaseStudy.Auto.Lang
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
import           Json

import           Core

-- autoFileBool :: FilePath
-- autoFileBool = "bench/AutoBench/Automotive02_merged_evolution_history_boolean.json"

-- | a different file that represents a possible json
-- smAutoFile :: FilePath
-- smAutoFile = "bench/AutoBench/vsat_small_example.json"

-- | a chunk of the large autoFile above
paulDebugFile :: FilePath
paulDebugFile = "bench/PaulDebug/vsat_small_chunk.json"
paulDebugFileBroken :: FilePath
paulDebugFileBroken = "bench/PaulDebug/vsat_small_chunk_broken.json"
jeffsFavorite :: FilePath
jeffsFavorite = "bench/PaulDebug/jeff_prefers_this_one.json"

-- main :: IO (V String (Maybe ThmResult))

testIfICanParse :: FilePath -> IO()
testIfICanParse file = do
    -- readfile is strict
  bJsn <- BS.readFile file
  let (Just bAuto) = decodeStrict bJsn :: Maybe Auto
      !bCs = constraints bAuto
      bPs' = parse langParser "" <$> bCs
      errors = lefts bPs'
      bPs = fmap (simplifyCtxs . renameCtxs sameCtxs) $ rights bPs'
  putStrLn "Success"
  putStrLn "Warnings:"
  mapM_ print errors

-- run with stack bench --profile vsat:auto --benchmark-arguments='+RTS -S -RTS --output timings.html'
main = do
  testIfICanParse jeffsFavorite

      -- | Hardcoding equivalencies in generated dimensions to reduce number of
      -- dimensions to 4
      -- sameDims :: Text -> Text
      -- sameDims d
      --   | d == "D_1" = "D_2"
      --   | d == "D_3" = "D_4"
      --   | otherwise = d

      -- bProp :: ReadableProp Text
      -- -- !bProp = ((renameDims sameDims) . naiveEncode . autoToVSat) $ autoAndJoin (bPs)
--       !bProp = (naiveEncode . autoToVSat) $ autoAndJoin (bPs)
--       !bPropOpts = applyOpts defConf bProp
--       autoNegConf = (Just $ toDimProp negConf)

--   -- Convert the fmf's to actual configurations
--   [ppV1]   <- genConfigPool d0Conf
--   [ppV2]   <- genConfigPool d2Conf
--   [ppV3]   <- genConfigPool d3Conf
--   [ppV4]   <- genConfigPool d4Conf
--   [ppVAll] <- genConfigPool dAllConf

--   [justV1] <- genConfigPool justV1Conf
--   [justV2] <- genConfigPool justV2Conf
--   [justV3] <- genConfigPool justV3Conf
--   [justV4] <- genConfigPool justV4Conf

--   [justV12] <- genConfigPool justV12Conf
--   [justV123] <- genConfigPool justV123Conf

--   -- | Compression ratio pairs
--   -- [justV12]  <- genConfigPool pD01Conf
--   -- [justV23]  <- genConfigPool pD12Conf
--   -- [justV34]  <- genConfigPool pD23Conf

--   confs' <- mkCompRatioConfs ds pairs
--   let [v01Conf,v12Conf,v23Conf] = confs'

--   let bPropV1   = selectVariantTotal ppV1 bProp
--       bPropV2   = selectVariantTotal ppV2 bProp
--       bPropV3   = selectVariantTotal ppV3 bProp
--       bPropV4   = selectVariantTotal ppV4 bProp
--       bPropVAll = selectVariantTotal ppVAll bProp

--       bPropJustV1 = selectVariant justV1 bProp
--       bPropJustV2 = selectVariant justV2 bProp
--       bPropJustV3 = selectVariant justV3 bProp
--       bPropJustV4 = selectVariant justV4 bProp
--       bPropJustV12 = selectVariant justV12 bProp
--       bPropJustV123 = selectVariant justV123 bProp

--       benches :: ReadableSMTConf Text -> [Benchmark]
--       benches solverConf = [
--         -- v - v
--          mkBench "v-->v" "V1"   d0Conf (satWithConf (toDimProp d0Conf) solverConf) bProp
--        , mkBench "v-->v" "V2" d2Conf (satWithConf (toDimProp d2Conf) solverConf) bProp
--        , mkBench "v-->v" "V3" d3Conf (satWithConf (toDimProp d3Conf) solverConf) bProp
--        , mkBench "v-->v" "V4" d4Conf (satWithConf (toDimProp d4Conf) solverConf) bProp
-- --        -- , mkBench' "v-->v" "EvolutionAware" (satWithConf (toDimProp sumConf) solverConf) bProp
--        , mkBench "v-->v" "V1*V2"        justV12Conf  (satWith solverConf) bPropJustV12
--        , mkBench "v-->v" "V1*V2*V3"     justV123Conf (satWith solverConf) bPropJustV123
--        , mkBench' "v-->v" "V1*V2*V3*V4"  (satWith solverConf) bProp

--         -- -- p - v
--        , mkBench "p-->v" "V1"  justV1Conf (pOnV solverConf) bPropV1
--        , mkBench "p-->v" "V2"  justV2Conf (pOnV solverConf) bPropV2
--        , mkBench "p-->v" "V3"  justV3Conf (pOnV solverConf) bPropV3
--        , mkBench "p-->v" "V4"  justV4Conf (pOnV solverConf) bPropV4
-- --        -- , mkBench' "p-->v" "EvolutionAware" (pOnVWithConf (toDimProp sumConf) solverConf) bProp
--        , mkBench "p-->v" "V1*V2"        justV12Conf (pOnV solverConf) bPropJustV12
--        , mkBench "p-->v" "V1*V2*V3"     justV123Conf (pOnV solverConf) bPropJustV123
--        , mkBench' "p-->v" "V1*V2*V3*V4"  (pOnV solverConf) bProp

--         -- -- p - p
--         , mkBench "p-->p" "V1"  justV1Conf (bfWith solverConf) bPropV1
--         , mkBench "p-->p" "V2"  justV2Conf (bfWith solverConf) bPropV2
--         , mkBench "p-->p" "V3"  justV3Conf (bfWith solverConf) bPropV3
--         , mkBench "p-->p" "V4"  justV4Conf (bfWith solverConf) bPropV4
--         -- , mkBench' "p-->p" "EvolutionAware"  (bfWithConf (toDimProp sumConf) solverConf) bProp
--         , mkBench "p-->p" "V1*V2"        justV12Conf (bfWith solverConf) bPropJustV12
--         , mkBench "p-->p" "V1*V2*V3"     justV123Conf (bfWith solverConf) bPropJustV123
--         , mkBench' "p-->p" "V1*V2*V3*V4"  (bfWith solverConf) bProp

--         -- v - p
--         , mkBench "v-->p" "V1"  justV1Conf (vOnPWithConf (toDimProp d0Conf) solverConf) bPropV1
--         , mkBench "v-->p" "V2"  justV2Conf (vOnPWithConf (toDimProp d2Conf) solverConf) bPropV2
--         , mkBench "v-->p" "V3"  justV3Conf (vOnPWithConf (toDimProp d3Conf) solverConf) bPropV3
--         , mkBench "v-->p" "V4"  justV4Conf (vOnPWithConf (toDimProp d4Conf) solverConf) bPropV4
--         -- , mkBench' "v-->p" "EvolutionAware"  (vOnPWithConf (toDimProp sumConf) solverConf) bProp
--         , mkBench "v-->p" "V1*V2"        justV12Conf (vOnPWith solverConf) bPropJustV12
--         , mkBench "v-->p" "V1*V2*V3"     justV123Conf (vOnPWith solverConf) bPropJustV123
--         , mkBench' "v-->p" "V1*V2*V3*V4"  (vOnPWith solverConf) bProp
--         ]

--     -- | Compression Ratio props
--       justbPropV12  = selectVariant v01Conf bProp
--       justbPropV23  = selectVariant v12Conf bProp
--       justbPropV34  = selectVariant v23Conf bProp

--       compRatioBenches :: ReadableSMTConf Text -> [Benchmark]
--       compRatioBenches solverConf =
--         [
--           -- v --> v
--           mkCompBench "v-->v" "V1*V2"  (satWithConf (toDimProp pD01Conf) solverConf) justbPropV12
--         , mkCompBench "v-->v" "V2*V3"  (satWithConf (toDimProp pD12Conf) solverConf) justbPropV23
--         , mkCompBench "v-->v" "V3*V4"  (satWithConf (toDimProp pD23Conf) solverConf) justbPropV34

--           -- v --> p
--         , mkCompBench "v-->p" "V1*V2"  (vOnPWithConf (toDimProp pD01Conf) solverConf) justbPropV12
--         , mkCompBench "v-->p" "V2*V3"  (vOnPWithConf (toDimProp pD12Conf) solverConf) justbPropV23
--         , mkCompBench "v-->p" "V3*V4"  (vOnPWithConf (toDimProp pD23Conf) solverConf) justbPropV34

--           -- p --> v
--         , mkCompBench "p-->v" "V1*V2"  (pOnVWithConf (toDimProp pD01Conf) solverConf) justbPropV12
--         , mkCompBench "p-->v" "V2*V3"  (pOnVWithConf (toDimProp pD12Conf) solverConf) justbPropV23
--         , mkCompBench "p-->v" "V3*V4"  (pOnVWithConf (toDimProp pD23Conf) solverConf) justbPropV34

--           -- p --> p
--         , mkCompBench "p-->p" "V1*V2"  (bfWithConf (toDimProp pD01Conf) solverConf) justbPropV12
--         , mkCompBench "p-->p" "V2*V3"  (bfWithConf (toDimProp pD12Conf) solverConf) justbPropV23
--         , mkCompBench "p-->p" "V3*V4"  (bfWithConf (toDimProp pD23Conf) solverConf) justbPropV34
--         ]

--   defaultMainWith benchConfig $
--     [  bgroup "Z3" (benches z3DefConf)
--       -- bgroup "Z3" (compRatioBenches z3DefConf)
--     , bgroup "Yices" (benches yicesDefConf)
--     , bgroup "CVC4" (benches cvc4DefConf)
--     , bgroup "Boolector" (benches boolectorDefConf)
--     ]

  -- (satWith z3DefConf) bProp >>= encodeFile "data/auto_vmodel.json"
