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
import           Data.List               (sort,delete,intersperse)
import           Data.Map                (size, Map, (!))
import qualified Data.SBV                as S
import qualified Data.SBV.Control        as SC
import qualified Data.SBV.Internals      as SI
import           Data.Text               (pack, unpack,Text)
import qualified Data.Text.IO            as T (writeFile)
import           System.IO
import           Text.Megaparsec         (parse)

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

import           Core

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

sliceAndNegate n xs = fromList (&&&) $ bnot <$> drop n xs

ds :: [VProp Text String String]
ds = bRef <$> ["D_0","D_2","D_4","D_5"]
-- D_0 /\    D_2   /\     D_4   /\  D_5
-- <0 /\ <=0 /\ <1 /\ <=1 /\ <2 /\ <= 2

[d0, d2, d4, d5] = ds

-- dimConf' :: VProp Text String String
-- encoding for 6 configs that make sure the inequalities encompass each other
sumConf = (d0 &&& fromList (&&&) (bnot <$> tail ds)) -- <0
          ||| ((bnot d0) &&& d2 &&& (bnot d4 &&& bnot d5))   -- <0 /\ <1
          ||| ((bnot d0)&&& (bnot d2) &&& d4 &&& bnot d5) -- <0 /\ <1 /\
          ||| ((bnot d0)&&& (bnot d2) &&& (bnot d4) &&& d5) -- <0 /\ <1 /\

-- | Configs that select only one version
d0Conf = (d0 &&& fromList (&&&) (bnot <$> tail ds)) -- <0
d2Conf = ((bnot d0) &&& d2 &&& (bnot d4 &&& bnot d5))   -- <0 /\ <1
d3Conf = ((bnot d0) &&& (bnot d2) &&& d4 &&& bnot d5) -- <0 /\ <1 /\
d4Conf = ((bnot d0) &&& (bnot d2) &&& (bnot d4) &&& d5) -- <0 /\ <1 /\
dAllConf = (d0 &&& d2 &&& d4 &&& d5) -- <0 /\ <1 /\

-- | Configs that remove choices and leave that particular choice
justV1Conf = (bnot d2) &&& (bnot d4) &&& (bnot d5)
justV2Conf = (bnot d0) &&& (bnot d4) &&& (bnot d5)
justV3Conf = (bnot d0) &&& (bnot d2) &&& (bnot d5)
justV4Conf = (bnot d0) &&& (bnot d2) &&& (bnot d4)

justV12Conf = (bnot d4) &&& (bnot d5)
justV123Conf = (bnot d5)

negConf = conjoin $ bnot <$> ds

baselineSolve :: [AutoLang Text Text] -> IO S.SatResult
baselineSolve props = S.runSMT $
  do assocMap <- makeAssocMap props
     SC.query $
       do
         (assocMap ! plainHandle) >>= S.constrain
         fmap S.SatResult SC.getSMTResult

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
      sameDims :: Text -> Text
      sameDims d
        | d == "D_1" = "D_2"
        | d == "D_3" = "D_4"
        | otherwise = d

      !bProp = ((renameDims sameDims) . naiveEncode . autoToVSat) $ autoAndJoin (bPs)
      !bPropOpts = applyOpts defConf bProp
      autoNegConf = (Just $ toDimProp negConf)

  -- Convert the fmf's to actual configurations
  [ppV1]   <- genConfigPool d0Conf
  [ppV2]   <- genConfigPool d2Conf
  [ppV3]   <- genConfigPool d3Conf
  [ppV4]   <- genConfigPool d4Conf
  [ppVAll] <- genConfigPool dAllConf

  [justV1] <- genConfigPool justV1Conf
  [justV2] <- genConfigPool justV2Conf
  [justV3] <- genConfigPool justV3Conf
  [justV4] <- genConfigPool justV4Conf

  [justV12] <- genConfigPool justV12Conf
  [justV123] <- genConfigPool justV123Conf

  let !bPropV1   = selectVariantTotal ppV1 bProp
      !bPropV2   = selectVariantTotal ppV2 bProp
      !bPropV3   = selectVariantTotal ppV3 bProp
      !bPropV4   = selectVariantTotal ppV4 bProp
      !bPropVAll = selectVariantTotal ppVAll bProp

      (Just bPropJustV1) = selectVariant justV1 bProp
      (Just bPropJustV2) = selectVariant justV2 bProp
      (Just bPropJustV3) = selectVariant justV3 bProp
      (Just bPropJustV4) = selectVariant justV4 bProp
      (Just bPropJustV12) = selectVariant justV12 bProp
      (Just bPropJustV123) = selectVariant justV123 bProp

      benches :: ReadableSMTConf Text -> [Benchmark]
      benches solverConf = [
        -- v - v
        mkBench "v-->v" "V1"   d0Conf (satWithConf (toDimProp d0Conf) solverConf) bProp
        , mkBench "v-->v" "V2" d2Conf (satWithConf (toDimProp d2Conf) solverConf) bProp
        , mkBench "v-->v" "V3" d3Conf (satWithConf (toDimProp d3Conf) solverConf) bProp
        , mkBench "v-->v" "V4" d4Conf (satWithConf (toDimProp d4Conf) solverConf) bProp
        , mkBench "v-->v" "EvolutionAware" sumConf (satWithConf (toDimProp sumConf) solverConf) bProp
        -- , mkBench "v-->v" "V1*V2"        (satWith solverConf) bPropJustV12
        -- , mkBench "v-->v" "V1*V2*V3"     (satWith solverConf) bPropJustV123
        -- , mkBench "v-->v" "V1*V2*V3*V4"  (satWith solverConf) bProp

        --   -- p - v
        -- , mkBench "p-->v" "V1"  (pOnVWithConf Nothing solverConf) bPropV1
        -- , mkBench "p-->v" "V2"  (pOnVWithConf Nothing solverConf) bPropV2
        -- , mkBench "p-->v" "V3"  (pOnVWithConf Nothing solverConf) bPropV3
        -- , mkBench "p-->v" "V4"  (pOnVWithConf Nothing solverConf) bPropV4
        -- , mkBench "p-->v" "EvolutionAware" (pOnVWithConf (toDimProp sumConf) solverConf) bProp

        --            -- p - p
        -- , mkBench "p-->p" "V1"  (bfWith solverConf) bPropV1
        -- , mkBench "p-->p" "V2"  (bfWith solverConf) bPropV2
        -- , mkBench "p-->p" "V3"  (bfWith solverConf) bPropV3
        -- , mkBench "p-->p" "V4"  (bfWith solverConf) bPropV4

        --            -- v - p
        -- , mkBench "v-->p" "V1"  (bfWithConf (toDimProp d0Conf) solverConf) bProp
        -- , mkBench "v-->p" "V2"  (bfWithConf (toDimProp d2Conf) solverConf) bProp
        -- , mkBench "v-->p" "V3"  (bfWithConf (toDimProp d3Conf) solverConf) bProp
        -- , mkBench "v-->p" "V4"  (bfWithConf (toDimProp d4Conf) solverConf) bProp
        -- , mkBench "v-->p" "EvolutionAware"  (bfWithConf (toDimProp sumConf) solverConf) bProp
        -- , mkBench "v-->p" "V1*V2"        (bfWith solverConf) bPropJustV12
        -- , mkBench "v-->p" "V1*V2*V3"     (bfWith solverConf) bPropJustV123
        -- , mkBench "v-->p" "V1*V2*V3*V4"  (bfWith solverConf) bProp
        ]
  -- res' <- runIncrementalSolve bPs

  -- mdl <- baselineSolve bPs
  -- print mdl
  -- putStrLn $ "Done with parse: "
  -- mapM_ (putStrLn . show) $ (sPs)
  -- putStrLn $! show bProp
  -- putStrLn $ "------------------"
  -- putStrLn $ "Solving: "
  -- res' <- satWithConf (toDimProp d0Conf) emptyConf bProp
  -- res' <- ad id bProp
  -- res' <- bfWithConf (toDimProp d0Conf) emptyConf bProp
  -- res' <- satWith emptyConf sProp
  -- putStrLn "DONE!"
  -- print $ (length $ show res')
  -- print "done"
  -- let !p = prop 6000
  -- print $ length p
  -- -- res <- test 10
  -- res <- S.runSMT $ do p' <- mapM S.sBool p
  --                      SC.query $! test' p'
  -- putStrLn "Running Good:\n"
  -- goodRes <- testS goodS 1000

  defaultMain
    [ bgroup "Z3" (benches z3DefConf)
    -- , bgroup "CVC4" (benches cvc4DefConf)
    -- , bgroup "Yices" (benches yicesDefConf)
    -- , bgroup "Boolector" (benches boolectorDefConf)
    ]
