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
import           Data.List               (sort,delete)
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

d2Conf = ((bnot d0) &&& d2 &&& (bnot d4 &&& bnot d5))   -- <0 /\ <1

d4Conf = ((bnot d0)&&& (bnot d2) &&& d4 &&& bnot d5) -- <0 /\ <1 /\

d5Conf = ((bnot d0)&&& (bnot d2) &&& (bnot d4) &&& d5) -- <0 /\ <1 /\

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


  -- res' <- runIncrementalSolve bPs

  -- putStrLn $ "Done with parse: "
  -- mapM_ (putStrLn . show) $ (sPs)
  -- putStrLn $! show bProp
  -- putStrLn $ "------------------"
  -- putStrLn $ "Solving: "
  -- res' <- satWithConf (toAutoConf d0Conf) emptyConf bProp
  -- res' <- ad id bProp
  -- res' <- satWithConf autoConf emptyConf (bRef "a" &&& bRef "c")
  -- res' <- satWith emptyConf sProp
  -- putStrLn "DONE!"
  -- print $ res'
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
    bgroup "vsat" [--  bench "small file:NoOpts"  . nfIO $ satWithConf Nothing emptyConf sProp
                   -- bench "small file:DefOpts" . nfIO $ satWith defConf   sProp
                   -- , bench "small file:Empty:Compact" . nfIO $ satWith defConf   (compactEncode sPs)
                     bench "Auto:VSolve:V1"  . nfIO $ satWithConf (toAutoConf d0Conf) emptyConf bProp
                   , bench "Auto:VSolve:V2"  . nfIO $ satWithConf (toAutoConf d2Conf) emptyConf bProp
                   , bench "Auto:VSolve:V3"  . nfIO $ satWithConf (toAutoConf d4Conf) emptyConf bProp
                   , bench "Auto:VSolve:V4"  . nfIO $ satWithConf (toAutoConf d5Conf) emptyConf bProp
                   , bench "Auto:VSolve:Sum"  . nfIO $ satWithConf (toAutoConf sumConf) emptyConf bProp
                   -- , bench "Auto:VSolve:DefOpts" . nfIO $ satWithConf autoConf emptyConf bPropOpts
                   -- , bench "Auto:IncrementalBaseline:Naive" . nfIO $ runIncrementalSolve bPs
                   -- bench "Auto:IncrementalBaseline:Compact" . nfIO $! satWith emptyConf (compactEncode bPs)
                  ]
    ]
