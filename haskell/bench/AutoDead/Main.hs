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

-- | Compression Ratio setup
[pD01Conf, pD12Conf, pD23Conf] = mkCompRatioPairs ds pairs

pairs = mkPairs ds

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
      features = vars bProp'
      bPropLength = Set.size features

  let
    (featIndex,_) = randomR (0, bPropLength) (mkStdGen 1111)
    deadFeature   = Set.elemAt featIndex features
  -- construct the dead feature proposition
    bProp         = bProp' &&& (bRef deadFeature)

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

  -- | Compression ratio pairs
  -- [justV12]  <- genConfigPool pD01Conf
  -- [justV23]  <- genConfigPool pD12Conf
  -- [justV34]  <- genConfigPool pD23Conf

  confs' <- mkCompRatioConfs ds pairs
  let [v01Conf,v12Conf,v23Conf] = confs'

  let bPropV1   = selectVariantTotal ppV1 bProp
      bPropV2   = selectVariantTotal ppV2 bProp
      bPropV3   = selectVariantTotal ppV3 bProp
      bPropV4   = selectVariantTotal ppV4 bProp
      bPropVAll = selectVariantTotal ppVAll bProp

      bPropJustV1 = selectVariant justV1 bProp
      bPropJustV2 = selectVariant justV2 bProp
      bPropJustV3 = selectVariant justV3 bProp
      bPropJustV4 = selectVariant justV4 bProp
      bPropJustV12 = selectVariant justV12 bProp
      bPropJustV123 = selectVariant justV123 bProp

      benches :: ReadableSMTConf Text -> [Benchmark]
      benches solverConf = [
        -- v - v
          mkBench "v-->v" "V1*V2"        justV12Conf  (satWith solverConf) bPropJustV12
        , mkBench "v-->v" "V1*V2*V3"     justV123Conf (satWith solverConf) bPropJustV123
        , mkBench' "v-->v" "V1*V2*V3*V4"  (satWith solverConf) bProp

        -- -- p - v
        , mkBench "p-->v" "V1*V2"        justV12Conf (pOnV solverConf) bPropJustV12
        , mkBench "p-->v" "V1*V2*V3"     justV123Conf (pOnV solverConf) bPropJustV123
        , mkBench' "p-->v" "V1*V2*V3*V4"  (pOnV solverConf) bProp

        -- -- p - p
        , mkBench "p-->p" "V1*V2"        justV12Conf (bfWith solverConf) bPropJustV12
        , mkBench "p-->p" "V1*V2*V3"     justV123Conf (bfWith solverConf) bPropJustV123
        , mkBench' "p-->p" "V1*V2*V3*V4"  (bfWith solverConf) bProp

        -- v - p
        , mkBench "v-->p" "V1*V2"        justV12Conf (vOnPWith solverConf) bPropJustV12
        , mkBench "v-->p" "V1*V2*V3"     justV123Conf (vOnPWith solverConf) bPropJustV123
        , mkBench' "v-->p" "V1*V2*V3*V4"  (vOnPWith solverConf) bProp
        ]

    -- | Compression Ratio props
      justbPropV12  = selectVariant v01Conf bProp
      justbPropV23  = selectVariant v12Conf bProp
      justbPropV34  = selectVariant v23Conf bProp

      compRatioBenches :: ReadableSMTConf Text -> [Benchmark]
      compRatioBenches solverConf =
        [
          -- v --> v
          mkCompBench "v-->v" "V1*V2"  (satWithConf (toDimProp pD01Conf) solverConf) justbPropV12
        , mkCompBench "v-->v" "V2*V3"  (satWithConf (toDimProp pD12Conf) solverConf) justbPropV23
        , mkCompBench "v-->v" "V3*V4"  (satWithConf (toDimProp pD23Conf) solverConf) justbPropV34

          -- v --> p
        , mkCompBench "v-->p" "V1*V2"  (vOnPWithConf (toDimProp pD01Conf) solverConf) justbPropV12
        , mkCompBench "v-->p" "V2*V3"  (vOnPWithConf (toDimProp pD12Conf) solverConf) justbPropV23
        , mkCompBench "v-->p" "V3*V4"  (vOnPWithConf (toDimProp pD23Conf) solverConf) justbPropV34

          -- p --> v
        , mkCompBench "p-->v" "V1*V2"  (pOnVWithConf (toDimProp pD01Conf) solverConf) justbPropV12
        , mkCompBench "p-->v" "V2*V3"  (pOnVWithConf (toDimProp pD12Conf) solverConf) justbPropV23
        , mkCompBench "p-->v" "V3*V4"  (pOnVWithConf (toDimProp pD23Conf) solverConf) justbPropV34

          -- p --> p
        , mkCompBench "p-->p" "V1*V2"  (bfWithConf (toDimProp pD01Conf) solverConf) justbPropV12
        , mkCompBench "p-->p" "V2*V3"  (bfWithConf (toDimProp pD12Conf) solverConf) justbPropV23
        , mkCompBench "p-->p" "V3*V4"  (bfWithConf (toDimProp pD23Conf) solverConf) justbPropV34
        ]
  -- mdl <- baselineSolve bPs
  -- print mdl
  -- putStrLn $ "Done with parse: "
  -- mapM_ (putStrLn . show) $ (sPs)
  -- putStrLn $! show bProp
  putStrLn $ "------------------"
  putStrLn $ "Solving: "
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
  -- genConfigPool pD01Conf >>= putStrLn . show
  -- putStrLn $ show $ mkCompRatioPairs ds pairs
  -- putStrLn "Running Good:\n"
  -- goodRes <- testS goodS 1000

  defaultMain
    [  bgroup "Z3" (benches z3DefConf)
      -- bgroup "Z3" (compRatioBenches z3DefConf)
    -- , bgroup "CVC4" (benches cvc4DefConf)
    -- , bgroup "Yices" (benches yicesDefConf)
    -- , bgroup "Boolector" (benches boolectorDefConf)
    ]
