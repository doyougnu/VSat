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
import           Data.Map                (size, Map)
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

sliceAndNegate n xs = fromList' (&&&) $ bnot <$> drop n xs

ds :: [VProp Text String String]
ds = bRef <$> ["D_0","D_2","D_4","D_5"]
-- D_0 /\    D_2   /\     D_4   /\  D_5
-- <0 /\ <=0 /\ <1 /\ <=1 /\ <2 /\ <= 2

[d0, d2, d4, d5] = ds

-- dimConf' :: VProp Text String String
-- encoding for 6 configs that make sure the inequalities encompass each other
sumConf = (d0 &&& fromList' (&&&) (bnot <$> tail ds)) -- <0
          ||| ((bnot d0) &&& d2 &&& (bnot d4 &&& bnot d5))   -- <0 /\ <1
          ||| ((bnot d0)&&& (bnot d2) &&& d4 &&& bnot d5) -- <0 /\ <1 /\
          ||| ((bnot d0)&&& (bnot d2) &&& (bnot d4) &&& d5) -- <0 /\ <1 /\

d0Conf = (d0 &&& fromList' (&&&) (bnot <$> tail ds)) -- <0

d02Conf = (d0 &&& d2 &&& (bnot d4 &&& bnot d5))   -- <0 /\ <1

d024Conf = (d0 &&& d2 &&& d4 &&& bnot d5) -- <0 /\ <1 /\

dAllConf = (d0 &&& d2 &&& d4 &&& d5) -- <0 /\ <1 /\

d2Conf = ((bnot d0) &&& d2 &&& (bnot d4 &&& bnot d5))   -- <0 /\ <1

d4Conf = ((bnot d0) &&& (bnot d2) &&& d4 &&& bnot d5) -- <0 /\ <1 /\

negConf = conjoin $ bnot <$> ds

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
      sPs = fmap (simplifyCtxs . renameCtxs sameCtxs) $ rights sPs'

      bPs' = parse langParser "" <$> bCs
      bPsSimp = fmap (simplifyCtxs . renameCtxs sameCtxs) $ rights bPs'
      bPs = rights bPs'

      -- | Hardcoding equivalencies in generated dimensions to reduce number of
      -- dimensions to 4
      sameDims :: Text -> Text
      sameDims d
        | d == "D_1" = "D_2"
        | d == "D_3" = "D_4"
        | otherwise = d

      !sProp = ((renameDims sameDims) . naiveEncode . autoToVSat) $ autoAndJoin sPs
      --  -- take 4500 bPs produces a solution for the plain case (all dims set to false)
      !bProp = ((renameDims sameDims) . naiveEncode . autoToVSat) $ autoAndJoin (bPs)
      !bPropOpts = applyOpts defConf bProp
      toAutoConf = Just . toDimProp
      autoNegConf = (Just $ toDimProp negConf)

      run !desc !f prop = bench desc $! nfIO (f prop)

      mkBench alg conf !f prop = run desc f prop
        where
          !desc' = ["Chc",show nChc , "numPlain", show nPln , "Compression", show ratio]
          !desc = mconcat $ intersperse "/" $ pure alg ++ pure conf ++ desc'
          !nPln = numPlain prop
          !nChc = numChc prop
          ratio :: Double
          !ratio = fromRational $ compressionRatio prop

  [ppV1] <- genConfigPool d0Conf
  [ppV12] <- genConfigPool d02Conf
  [ppV124] <- genConfigPool d024Conf
  [ppVAll] <- genConfigPool dAllConf

  let !bPropV1 = selectVariantTotal ppV1 bProp
      !bPropV12 = selectVariantTotal ppV12 bProp
      !bPropV124 = selectVariantTotal ppV124 bProp
      !bPropVAll = selectVariantTotal ppVAll bProp

  -- res' <- runIncrementalSolve bPs

  -- putStrLn $ "Done with parse: "
  -- mapM_ (putStrLn . show) $ (sPs)
  -- putStrLn $! show bProp
  -- putStrLn $ "------------------"
  -- putStrLn $ "Solving: "
  -- res' <- satWithConf (toAutoConf d0Conf) emptyConf bProp
  -- res' <- ad id bProp
  -- res' <- bfWithConf (toAutoConf d0Conf) emptyConf bProp
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
    [
    bgroup "Auto" [
        -- v - v
                     mkBench "VSolve" "V1"  (satWithConf (toAutoConf d0Conf) emptyConf) bProp
                   , mkBench "VSolve" "V1+V2"  (satWithConf (toAutoConf d02Conf) emptyConf) bProp
                   , mkBench "VSolve" "V1+V2+V3"  (satWithConf (toAutoConf d024Conf) emptyConf) bProp
                   , mkBench "VSolve" "V1+V2+V3+V4"  (satWithConf (toAutoConf dAllConf) emptyConf) bProp
                   -- p - v
                   , mkBench "PlainOnVSat" "V1"  (pOnVWithConf Nothing) bPropV1
                   , mkBench "PlainOnVSat" "V1+V2"  (pOnVWithConf Nothing) bPropV12
                   , mkBench "PlainOnVSat" "V1+V2+V3"  (pOnVWithConf Nothing) bPropV124
                   , mkBench "PlainOnVSat" "V1+V2+V3+V4"  (pOnVWithConf Nothing) bPropVAll

                   -- p - p
                   , mkBench "BruteForce" "V1"  (bfWith emptyConf) bPropV1
                   , mkBench "BruteForce" "V1+V2"  (bfWith emptyConf) bPropV12
                   , mkBench "BruteForce" "V1+V2+V3"  (bfWith emptyConf) bPropV124
                   , mkBench "BruteForce" "V1+V2+V3+V4"  (bfWith emptyConf) bPropVAll

                   -- v - p
                   , mkBench "VariationalOnPlain" "V1"  (bfWithConf (toAutoConf d0Conf) emptyConf) bProp
                   , mkBench "VariationalOnPlain" "V1+V2"  (bfWithConf (toAutoConf d02Conf) emptyConf) bProp
                   , mkBench "VariationalOnPlain" "V1+V2+V3"  (bfWithConf (toAutoConf d024Conf) emptyConf) bProp
                   , mkBench "VariationalOnPlain" "V1+V2+V3+V4"  (bfWithConf (toAutoConf dAllConf) emptyConf) bProp
                  ]
    ]

                   --   bench "Auto:VSolve:NoConf"  . nfIO $ satWithConf Nothing emptyConf bProp
                   -- , bench "Auto:PonV:NoConf"  . nfIO $ pOnVWithConf Nothing bProp
                   -- , bench "Auto:BF:NoConf"  . nfIO $ bfWith emptyConf bProp
