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
ds = bRef <$> ["D_0","D_1","D_2","D_3","D_4","D_5"]

[d0, d1, d2, d3, d4, d5] = ds

-- dimConf' :: VProp Text String String
-- encoding for 6 configs that make sure the inequalities encompass each other
dimConf = (d0 &&& fromList' (&&&) (bnot <$> tail ds)) -- <0
          ||| ((d0 &&& d1) &&& sliceAndNegate 2 ds)   -- <0 /\ <=0
          ||| (d0 &&& d1 &&& d4 &&& (bnot d5 &&& bnot d2 &&& bnot d3)) -- <0 /\ <=0 /\ <1
          ||| (d0 &&& d1 &&& d4 &&& d5 &&& (bnot d2 &&& bnot d3)) -- <0 /\ <=0 /\ <1 /\ <=1
          ||| (d0 &&& d1 &&& d2 &&& d4 &&& d5 &&& bnot d3)  -- <0 /\ <=0 /\ <1 /\ <=1 /\ <2
          ||| fromList' (&&&) ds -- <0 /\ <=0 /\ <1 /\ <=1 /\ <2 /\ <= 2

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
      sPs = rights sPs'

      bPs' = parse langParser "" <$> bCs
      bPs = rights bPs'

      !sProp = (naiveEncode . autoToVSat) $ autoAndJoin sPs
      --  -- take 4500 bPs produces a solution for the plain case (all dims set to false)
      !bProp = (naiveEncode . autoToVSat) $ autoAndJoin (take 500 bPs)
      !bPropOpts = applyOpts defConf bProp
      autoConf = (Just $ toDimProp dimConf)


  -- res' <- runIncrementalSolve sPs
  -- res' <- satWithConf autoConf  emptyConf bProp
  -- print $ res'
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
                     bench "Auto:VSolve:NoOpts"  . nfIO $ satWithConf autoConf emptyConf bProp
                   , bench "Auto:VSolve:DefOpts" . nfIO $ satWithConf autoConf emptyConf bPropOpts
                   , bench "Auto:IncrementalBaseline:Naive" . nfIO $ runIncrementalSolve (take 500 bPs)
                   -- bench "Auto:IncrementalBaseline:Compact" . nfIO $! satWith emptyConf (compactEncode bPs)
                  ]
    ]
