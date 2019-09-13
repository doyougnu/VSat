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

-- main :: IO (V String (Maybe ThmResult))

        -- d == "D_16" = "D_1"
        -- d == "D_12" = "D_17"
        -- d == "D_6" = "D_13"
        -- d == "D_2" = "D_7"
        -- d == "D_10" = "D_3"
        -- d == "D_4" = "D_11"
        -- d == "D_8" = "D_5"
        -- d == "D_14" = "D_9"

sliceAndNegate n xs = fromList (&&&) $ bnot <$> drop n xs

ds :: [VProp Text String String]
ds = bRef <$> ["D_0", "D_1", "D_17", "D_13", "D_7", "D_3", "D_11", "D_5", "D_9", "D_15"]

[d0, d1, d17, d13, d7, d3, d11, d5, d9, d15] = ds

mkCascadeConf n xs = conjoin $ (take n xs) ++ (bnot <$> drop n xs)
mkMultConf n xs = conjoin (bnot <$> drop n xs)

-- d0Conf = mkCascadeConf 1 ds
-- d01Conf = mkCascadeConf 2 ds
-- d012Conf = mkCascadeConf 3 ds
-- d0123Conf = mkCascadeConf 4 ds
-- d01234Conf = mkCascadeConf 5 ds
-- d012345Conf = mkCascadeConf 6 ds
-- d0123456Conf = mkCascadeConf 7 ds
-- d01234567Conf = mkCascadeConf 8 ds
-- d012345678Conf = mkCascadeConf 9 ds
-- d0123456789Conf = mkCascadeConf 10 ds

-- | Choice preserving confs
-- justD0Conf = conjoin $ bnot <$> delete d0 ds
-- justD1Conf = conjoin $ bnot <$> delete d1 ds
-- justD2Conf = conjoin $ bnot <$> delete d17 ds
-- justD3Conf = conjoin $ bnot <$> delete d13 ds
-- justD4Conf = conjoin $ bnot <$> delete d1 ds
-- justD5Conf = conjoin $ bnot <$> delete d17 ds
-- justD6Conf = conjoin $ bnot <$> delete d13 ds
-- justD7Conf = conjoin $ bnot <$> delete d1 ds
-- justD8Conf = conjoin $ bnot <$> delete d17 ds
-- justD9Conf = conjoin $ bnot <$> delete d13 ds
justD01Conf = mkMultConf 2 ds
justD012Conf = mkMultConf 3 ds
justD0123Conf = mkMultConf 4 ds
justD01234Conf = mkMultConf 5 ds
justD012345Conf = mkMultConf 6 ds
justD0123456Conf = mkMultConf 7 ds
justD01234567Conf = mkMultConf 8 ds
justD012345678Conf = mkMultConf 9 ds

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
      -- bPsSimp = fmap (simplifyCtxs . renameCtxs sameCtxs) $ rights bPs'
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

      !bProp = ((renameDims sameDims) . naiveEncode . autoToVSat) $ autoAndJoin bPs
      dmapping = getDimMap $ autoAndJoin (bPs)
      !bPropOpts = applyOpts defConf bProp

  -- | convert choice preserving fmfs to actual confs
  -- [justV1]         <- genConfigPool justD0Conf
  -- [justV2]         <- genConfigPool justD1Conf
  -- [justV3]         <- genConfigPool justD2Conf
  -- [justV4]         <- genConfigPool justD3Conf
  [justV12]        <- genConfigPool justD01Conf
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

  let
    -- | choice preserving props
    -- (Just justbPropV1)         = selectVariant justV1 bProp
    -- (Just justbPropV2)         = selectVariant justV2 bProp
    -- (Just justbPropV3)         = selectVariant justV3 bProp
    -- (Just justbPropV4)         = selectVariant justV4 bProp
    (Just justbPropV12)        = selectVariant justV12 bProp
    (Just justbPropV123)       = selectVariant justV123 bProp
    (Just justbPropV1234)      = selectVariant justV1234 bProp
    (Just justbPropV12345)     = selectVariant justV12345 bProp
    (Just justbPropV123456)    = selectVariant justV123456 bProp
    (Just justbPropV1234567)   = selectVariant justV1234567 bProp
    (Just justbPropV12345678)  = selectVariant justV12345678 bProp
    (Just justbPropV123456789) = selectVariant justV123456789 bProp

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
      , mkBench "p-->v" "V1" d0Conf (pOnVWithConf  Nothing solverConf) bPropV1
      , mkBench "p-->v" "V2" d1Conf (pOnVWithConf  Nothing solverConf) bPropV2
      , mkBench "p-->v" "V3" d2Conf (pOnVWithConf  Nothing solverConf) bPropV3
      , mkBench "p-->v" "V4" d3Conf (pOnVWithConf  Nothing solverConf) bPropV4
      , mkBench "p-->v" "V5" d4Conf (pOnVWithConf  Nothing solverConf) bPropV5
      , mkBench "p-->v" "V6" d5Conf (pOnVWithConf  Nothing solverConf) bPropV6
      , mkBench "p-->v" "V7" d6Conf (pOnVWithConf  Nothing solverConf) bPropV7
      , mkBench "p-->v" "V8" d7Conf (pOnVWithConf  Nothing solverConf) bPropV8
      , mkBench "p-->v" "V9" d8Conf (pOnVWithConf  Nothing solverConf) bPropV9
      , mkBench "p-->v" "V10"d9Conf  (pOnVWithConf Nothing solverConf) bPropV10
      , mkBench' "p-->v" "EvolutionAware" (pOnVWithConf (toDimProp evoAwareConf) solverConf) bProp
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
                  -- , mkBench "p-->p" "EvolutionAware" (bfWithConf (toDimProp evoAwareConf) solverConf) bProp
 -- v - p
      , mkBench "v-->p" "V1"  d0Conf (bfWithConf (toDimProp d0Conf) solverConf) bProp
      , mkBench "v-->p" "V2"  d1Conf (bfWithConf (toDimProp d1Conf) solverConf) bProp
      , mkBench "v-->p" "V3"  d2Conf (bfWithConf (toDimProp d2Conf) solverConf) bProp
      , mkBench "v-->p" "V4"  d3Conf (bfWithConf (toDimProp d3Conf) solverConf) bProp
      , mkBench "v-->p" "V5"  d4Conf (bfWithConf (toDimProp d4Conf) solverConf) bProp
      , mkBench "v-->p" "V6"  d5Conf (bfWithConf (toDimProp d5Conf) solverConf) bProp
      , mkBench "v-->p" "V7"  d6Conf (bfWithConf (toDimProp d6Conf) solverConf) bProp
      , mkBench "v-->p" "V8"  d7Conf (bfWithConf (toDimProp d7Conf) solverConf) bProp
      , mkBench "v-->p" "V9"  d8Conf (bfWithConf (toDimProp d8Conf) solverConf) bProp
      , mkBench "v-->p" "V10" d9Conf (bfWithConf (toDimProp d9Conf) solverConf) bProp
      , mkBench' "v-->p" "EvolutionAware" (bfWithConf (toDimProp evoAwareConf) solverConf) bProp

      , mkBench "v-->p" "V1*V2"                          justD01Conf (bfWith solverConf) justbPropV12
      , mkBench "v-->p" "V1*V2*V3"                       justD012Conf (bfWith solverConf) justbPropV123
      , mkBench "v-->p" "V1*V2*V3*V4"                    justD0123Conf (bfWith solverConf) justbPropV1234
      , mkBench "v-->p" "V1*V2*V3*V4*V5"                 justD01234Conf (bfWith solverConf) justbPropV12345
      , mkBench "v-->p" "V1*V2*V3*V4*V5*V6"              justD012345Conf (bfWith solverConf) justbPropV123456
      , mkBench "v-->p" "V1*V2*V3*V4*V5*V6*V7"           justD0123456Conf (bfWith solverConf) justbPropV1234567
      , mkBench "v-->p" "V1*V2*V3*V4*V5*V6*V7*V8"        justD01234567Conf (bfWith solverConf) justbPropV12345678
      , mkBench "v-->p" "V1*V2*V3*V4*V5*V6*V7*V8*V9"     justD012345678Conf (bfWith solverConf) justbPropV123456789
      , mkBench' "v-->p" "V1*V2*V3*V4*V5*V6*V7*V8*V9*V10" (bfWith solverConf) bProp
      ]

  -- res' <- runIncrementalSolve bPs

  -- putStrLn $ "Done with parse: "
  -- mapM_ (putStrLn . show . second numTerms) $
  --   zip [1..] [bPropV1, bPropV12, bPropV123, bPropV1234, bPropV12345, bPropV123456, bPropV123456, bPropV1234567, bPropV12345678, bPropV123456789, bPropVAll]
  -- putStrLn $ "------------------"
  --  utStrLn $ (show bProp)
  -- res' <- satWithConf (toDimProp d0Conf) emptyConf bProp
  -- res' <- ad id bProp
  -- res' <- satWithConf autoConf emptyConf (bRef "a" &&& bRef "c")
  -- res' <- satWith emptyConf sProp
  -- putStrLn "DONE!"
  -- mapM_ (putStrLn . show) $ confs
  -- putStrLn $ show evoAwareConf
  -- print $ res'
  -- print $ length (show res')
  -- let !p = prop 6000
  -- -- res <- test 10
  -- res <- S.runSMT $ do p' <- mapM S.sBool p
  --                      SC.query $! test' p'
  -- putStrLn "Running Good:\n"
  -- goodRes <- testS goodS 1000

  defaultMain
    [ -- bgroup "ABC" (benches abcDefConf)
    --   bgroup "Yices" (benches yicesDefConf)
    -- , bgroup "CVC4" (benches cvc4DefConf)
      bgroup "Z3" (benches z3DefConf)
    -- , bgroup "Boolector" (benches boolectorDefConf)
    ]

-- -- [d0Conf, d1Conf, d17Conf, d13Conf, d7Conf, d3Conf, d11Conf, d5Conf, d9Conf, d15Conf] = confs

-- d0Conf = mkCascadeConf 1 ds
-- d1Conf = mkCascadeConf 2 ds
-- d2Conf = mkCascadeConf 3 ds
-- d3Conf = mkCascadeConf 4 ds
-- d4Conf = mkCascadeConf 5 ds
-- d5Conf = mkCascadeConf 6 ds
-- d6Conf = mkCascadeConf 7 ds
-- d7Conf = mkCascadeConf 8 ds
-- d8Conf = mkCascadeConf 9 ds
-- d9Conf = mkCascadeConf 10 ds
