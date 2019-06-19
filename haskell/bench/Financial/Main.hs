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

sliceAndNegate n xs = fromList' (&&&) $ bnot <$> drop n xs

ds :: [VProp Text String String]
ds = bRef <$> ["D_0", "D_1", "D_17", "D_13", "D_7", "D_3", "D_11", "D_5", "D_9", "D_15"]

[d0, d1, d17, d13, d7, d3, d11, d5, d9, d15] = ds

mkCascadeConf n xs = conjoin $ (take n xs) ++ (bnot <$> drop n xs)

-- d0Conf = mkCascadeConf 1 ds
d01Conf = mkCascadeConf 2 ds
d012Conf = mkCascadeConf 3 ds
d0123Conf = mkCascadeConf 4 ds
d01234Conf = mkCascadeConf 5 ds
d012345Conf = mkCascadeConf 6 ds
d0123456Conf = mkCascadeConf 7 ds
d01234567Conf = mkCascadeConf 8 ds
d012345678Conf = mkCascadeConf 9 ds
d0123456789Conf = mkCascadeConf 10 ds
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

[d0Conf, d1Conf, d17Conf, d13Conf, d7Conf, d3Conf, d11Conf, d5Conf, d9Conf, d15Conf] = confs
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
      toAutoConf = Just . toDimProp

  [ppV1]         <- genConfigPool d0Conf
  [ppV12]        <- genConfigPool d01Conf
  [ppV123]       <- genConfigPool d012Conf
  [ppV1234]      <- genConfigPool d0123Conf
  [ppV12345]     <- genConfigPool d01234Conf
  [ppV123456]    <- genConfigPool d012345Conf
  [ppV1234567]   <- genConfigPool d0123456Conf
  [ppV12345678]  <- genConfigPool d01234567Conf
  [ppV123456789] <- genConfigPool d012345678Conf
  [ppVAll]       <- genConfigPool d0123456789Conf

  [ppV1]  <- genConfigPool d0Conf
  [ppV2]  <- genConfigPool d1Conf
  [ppV3]  <- genConfigPool d17Conf
  [ppV4]  <- genConfigPool d13Conf
  [ppV5]  <- genConfigPool d7Conf
  [ppV6]  <- genConfigPool d3Conf
  [ppV7]  <- genConfigPool d11Conf
  [ppV8]  <- genConfigPool d5Conf
  [ppV9]  <- genConfigPool d9Conf
  [ppV10] <- genConfigPool d15Conf

  let
    !bPropV1         = selectVariantTotal ppV1 bProp
    !bPropV12        = selectVariantTotal ppV12 bProp
    !bPropV123       = selectVariantTotal ppV123 bProp
    !bPropV1234      = selectVariantTotal ppV1234 bProp
    !bPropV12345     = selectVariantTotal ppV12345 bProp
    !bPropV123456    = selectVariantTotal ppV123456 bProp
    !bPropV1234567   = selectVariantTotal ppV1234567 bProp
    !bPropV12345678  = selectVariantTotal ppV12345678 bProp
    !bPropV123456789 = selectVariantTotal ppV123456789 bProp
    !bPropVAll       = selectVariantTotal ppVAll bProp

    !bPropV2  = selectVariantTotal ppV2  bProp
    !bPropV3  = selectVariantTotal ppV3  bProp
    !bPropV4  = selectVariantTotal ppV4  bProp
    !bPropV5  = selectVariantTotal ppV5  bProp
    !bPropV6  = selectVariantTotal ppV6  bProp
    !bPropV7  = selectVariantTotal ppV7  bProp
    !bPropV8  = selectVariantTotal ppV8  bProp
    !bPropV9  = selectVariantTotal ppV9  bProp
    !bPropV10 = selectVariantTotal ppV10 bProp

    run !desc !f prop = bench desc $! nfIO (f prop)

-- | keep a desc so that you can add to it later
    mkBench alg conf !f prop = run desc f prop
      where
        !desc' = ["Chc",show nChc , "numPlain", show nPln , "Compression", show ratio]
        !desc = mconcat $ intersperse "/" $ pure alg ++ pure conf ++ desc'
        !nPln = numPlain prop
        !nChc = numChc prop
        ratio :: Double
        !ratio = fromRational $ compressionRatio prop
  -- res' <- runIncrementalSolve bPs

  -- putStrLn $ "Done with parse: "
  -- mapM_ (putStrLn . show . second numTerms) $
  --   zip [1..] [bPropV1, bPropV12, bPropV123, bPropV1234, bPropV12345, bPropV123456, bPropV123456, bPropV1234567, bPropV12345678, bPropV123456789, bPropVAll]
  -- putStrLn $ "------------------"
  -- putStrLn $ (show bProp)
  -- res' <- satWithConf (toAutoConf d0Conf) emptyConf bProp
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
    [
    bgroup "Fin" [  -- mkBench "VSolve" "V1+V2" (satWithConf (toAutoConf d0Conf) emptyConf) bProp

  --                 , mkBench "VSolve" "V1" (satWithConf (toAutoConf d0Conf) emptyConf) bProp
  --                 , mkBench "VSolve" "V1+V2" (satWithConf (toAutoConf d01Conf) emptyConf) bProp
  --                 , mkBench "VSolve" "V1+V2+V3" (satWithConf (toAutoConf d012Conf) emptyConf) bProp
  --                 , mkBench "VSolve" "V1+V2+V3+V4" (satWithConf (toAutoConf d0123Conf) emptyConf) bProp
  --                 , mkBench "VSolve" "V1+V2+V3+V4+V5" (satWithConf (toAutoConf d01234Conf) emptyConf) bProp
  --                 , mkBench "VSolve" "V1+V2+V3+V4+V5+V6" (satWithConf (toAutoConf d012345Conf) emptyConf) bProp
  --                 , mkBench "VSolve" "V1+V2+V3+V4+V5+V6+V7" (satWithConf (toAutoConf d0123456Conf) emptyConf) bProp
  --                 , mkBench "VSolve" "V1+V2+V3+V4+V5+V6+V7+V8" (satWithConf (toAutoConf d01234567Conf) emptyConf) bProp
  --                 , mkBench "VSolve" "V1+V2+V3+V4+V5+V6+V7+V8+V9" (satWithConf (toAutoConf d012345678Conf) emptyConf) bProp
  --                 , mkBench "VSolve" "V1+V2+V3+V4+V5+V6+V7+V8+V9+V10" (satWithConf (toAutoConf d0123456789Conf) emptyConf) bProp
  -- p - v
                    mkBench "PlainOnVSAT" "V1" (pOnVWithConf  Nothing) bPropV1
                  , mkBench "PlainOnVSAT" "V2" (pOnVWithConf  Nothing) bPropV2
                  , mkBench "PlainOnVSAT" "V3" (pOnVWithConf  Nothing) bPropV3
                  , mkBench "PlainOnVSAT" "V4" (pOnVWithConf  Nothing) bPropV4
                  , mkBench "PlainOnVSAT" "V5" (pOnVWithConf  Nothing) bPropV5
                  , mkBench "PlainOnVSAT" "V6" (pOnVWithConf  Nothing) bPropV6
                  , mkBench "PlainOnVSAT" "V7" (pOnVWithConf  Nothing) bPropV7
                  , mkBench "PlainOnVSAT" "V8" (pOnVWithConf  Nothing) bPropV8
                  , mkBench "PlainOnVSAT" "V9" (pOnVWithConf  Nothing) bPropV9
                  , mkBench "PlainOnVSAT" "V10" (pOnVWithConf  Nothing) bPropV10
  -- p - p
                  , mkBench "BruteForce" "V1" (bfWith  emptyConf) bPropV1
                  , mkBench "BruteForce" "V2" (bfWith  emptyConf) bPropV2
                  , mkBench "BruteForce" "V3" (bfWith  emptyConf) bPropV3
                  , mkBench "BruteForce" "V4" (bfWith  emptyConf) bPropV4
                  , mkBench "BruteForce" "V5" (bfWith  emptyConf) bPropV5
                  , mkBench "BruteForce" "V6" (bfWith  emptyConf) bPropV6
                  , mkBench "BruteForce" "V7" (bfWith  emptyConf) bPropV7
                  , mkBench "BruteForce" "V8" (bfWith  emptyConf) bPropV8
                  , mkBench "BruteForce" "V9" (bfWith  emptyConf) bPropV9
                  , mkBench "BruteForce" "V10" (bfWith  emptyConf) bPropV10

  --                 , mkBench "VariationalOnPlain" "V1"  (bfWithConf (toAutoConf d0Conf) emptyConf) bProp
  --                 , mkBench "VariationalOnPlain" "V1+V2" (bfWithConf (toAutoConf d01Conf) emptyConf) bProp
  --                 , mkBench "VariationalOnPlain" "V1+V2+V3" (bfWithConf (toAutoConf d012Conf) emptyConf) bProp
  --                 , mkBench "VariationalOnPlain" "V1+V2+V3+V4" (bfWithConf (toAutoConf d0123Conf) emptyConf) bProp
  --                 , mkBench "VariationalOnPlain" "V1+V2+V3+V4+V5" (bfWithConf (toAutoConf d01234Conf) emptyConf) bProp
  --                 , mkBench "VariationalOnPlain" "V1+V2+V3+V4+V5+V6" (bfWithConf (toAutoConf d012345Conf) emptyConf) bProp
  --                 , mkBench "VariationalOnPlain" "V1+V2+V3+V4+V5+V6+V7" (bfWithConf (toAutoConf d0123456Conf) emptyConf) bProp
  --                 , mkBench "VariationalOnPlain" "V1+V2+V3+V4+V5+V6+V7+V8" (bfWithConf (toAutoConf d01234567Conf) emptyConf) bProp
  --                 , mkBench "VariationalOnPlain" "V1+V2+V3+V4+V5+V6+V7+V8+V9" (bfWithConf (toAutoConf d012345678Conf) emptyConf) bProp
  --                 , mkBench "VariationalOnPlain" "V1+V2+V3+V4+V5+V6+V7+V8+V9+V10" (bfWithConf (toAutoConf d0123456789Conf) emptyConf) bProp

                  -- , bench "VSolve:V2" . nfIO $ satWithConf (toAutoConf d1Conf) emptyConf bProp
                  -- , bench "VSolve:V3" . nfIO $ satWithConf (toAutoConf d17Conf) emptyConf bProp
                  -- , bench "VSolve:V4" . nfIO $ satWithConf (toAutoConf d13Conf) emptyConf bProp
                  -- , bench "VSolve:V5" . nfIO $ satWithConf (toAutoConf d7Conf) emptyConf bProp
                  -- , bench "VSolve:V6" . nfIO $ satWithConf (toAutoConf d3Conf) emptyConf bProp
                  -- , bench "VSolve:V7" . nfIO $ satWithConf (toAutoConf d11Conf) emptyConf bProp
                  -- , bench "VSolve:V8" . nfIO $ satWithConf (toAutoConf d5Conf) emptyConf bProp
                  -- , bench "VSolve:V9" . nfIO $ satWithConf (toAutoConf d9Conf) emptyConf bProp
                  -- , bench "VSolve:V10"  . nfIO $ satWithConf (toAutoConf d15Conf) emptyConf bProp
                  -- , bench "VSolve:Evo-Aware" . nfIO $ satWithConf (toAutoConf evoAwareConf) emptyConf bProp
                  ]
    -- ]

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
