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


  -- res' <- runIncrementalSolve bPs

  -- putStrLn $ "Done with parse: "
  -- mapM_ (putStrLn . show) $ (sPs)
  -- putStrLn $ "------------------"
  -- putStrLn $ "Solving: "
  -- res' <- satWithConf (toAutoConf d0Conf) emptyConf bProp
  -- res' <- ad id bProp
  -- res' <- satWithConf autoConf emptyConf (bRef "a" &&& bRef "c")
  -- res' <- satWith emptyConf sProp
  -- putStrLn "DONE!"
  -- mapM_ (putStrLn . show) $ confs
  -- putStrLn $ show evoAwareConf
  -- print "done"
  -- let !p = prop 6000
  -- -- res <- test 10
  -- res <- S.runSMT $ do p' <- mapM S.sBool p
  --                      SC.query $! test' p'
  -- putStrLn "Running Good:\n"
  -- goodRes <- testS goodS 1000

  defaultMain
    [
    bgroup "vsat" [
                     bench "Fin:VSolve:V1"  . nfIO $ satWithConf (toAutoConf d0Conf) emptyConf bProp
                   , bench "Fin:VSolve:V2"  . nfIO $ satWithConf (toAutoConf d1Conf) emptyConf bProp
                   , bench "Fin:VSolve:V3"  . nfIO $ satWithConf (toAutoConf d17Conf) emptyConf bProp
                   , bench "Fin:VSolve:V4"  . nfIO $ satWithConf (toAutoConf d13Conf) emptyConf bProp
                   , bench "Fin:VSolve:V5"  . nfIO $ satWithConf (toAutoConf d7Conf) emptyConf bProp
                   , bench "Fin:VSolve:V6"  . nfIO $ satWithConf (toAutoConf d3Conf) emptyConf bProp
                   , bench "Fin:VSolve:V7"  . nfIO $ satWithConf (toAutoConf d11Conf) emptyConf bProp
                   , bench "Fin:VSolve:V8"  . nfIO $ satWithConf (toAutoConf d5Conf) emptyConf bProp
                   , bench "Fin:VSolve:V9"  . nfIO $ satWithConf (toAutoConf d9Conf) emptyConf bProp
                   , bench "Fin:VSolve:V10"  . nfIO $ satWithConf (toAutoConf d15Conf) emptyConf bProp
                   , bench "Fin:VSolve:Evo-Aware" . nfIO $ satWithConf (toAutoConf evoAwareConf) emptyConf bProp
                  ]
    ]

-- [d0Conf, d1Conf, d17Conf, d13Conf, d7Conf, d3Conf, d11Conf, d5Conf, d9Conf, d15Conf] = confs
