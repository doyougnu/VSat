import           Control.Arrow           (first, second)
import           Criterion.Main
import           Criterion.Main.Options
import           Criterion.Types         (Config (..))
import           Data.Aeson              (decodeStrict')
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

linuxPaths :: FilePath
linuxPaths = "bench/Linux/"

linuxFiles = [ "2016-01-07.json"
             , "2016-01-09.json"
             , "2016-01-11.json"
             , "2016-01-12.json"
             , "2016-01-13.json"
             , "2016-01-14.json"
             , "2016-01-15.json"
             ]

files = fmap ((++) linuxPaths) linuxFiles

ds :: [VProp Text String String]
ds = bRef <$> ["D_0", "D_1", "D_2", "D_3", "D_4", "D_5", "D_6"]

[d0, d1, d2, d3, d4, d5, d6] = ds

-- | V1+V2
mkCascadeConf n xs = conjoin $ (take n xs) ++ (bnot <$> drop n xs)

-- | V1**V2
mkMultConf n xs = conjoin (bnot <$> drop n xs)

-- mkConf :: Int -> [VProp Text String String] -> VProp Text String String
-- mkConf n xs = conjoin $ (xs !! n) : (bnot <$> delete d xs)
--   where d = xs !! n

evoAwareConf = disjoin confs

mkConf x xs = x &&& (conjoin $ bnot <$> (delete x xs))

confs = fmap (flip mkConf ds) ds

d0Conf = mkConf d0 ds
d1Conf = mkConf d1 ds
d2Conf = mkConf d2 ds
d3Conf = mkConf d3 ds
d4Conf = mkConf d4 ds
d5Conf = mkConf d5 ds
d6Conf = mkConf d6 ds

-- main :: IO (V String (Maybe ThmResult))

toAutoConf = Just . toDimProp
-- run with stack bench --profile vsat:auto --benchmark-arguments='+RTS -S -RTS --output timings.html'
main = do
  -- readfile is strict
  ls' <- traverse BS.readFile files
  let (Just ls) = (traverse decodeStrict' ls') :: Maybe [Auto]
      !lConstraints = constraints <$> ls
      lLang = fmap (parse langParser "") <$> lConstraints
      lRight = concat $ rights <$> lLang
      lLeft =  lefts <$> lLang

      !lProps = ((naiveEncode . autoToVSat) . autoAndJoin) $ lRight
      !lProp  = lProps

      run !desc !f prop = bench desc $! nfIO (f prop)

      mkBench alg conf !f prop = run desc f prop
        where
          !desc' = ["Chc",show nChc , "numPlain", show nPln , "Compression", show ratio]
          !desc = mconcat $ intersperse "/" $ pure alg ++ pure conf ++ desc'
          !nPln = numPlain prop
          !nChc = numChc prop
          ratio :: Double
          !ratio = fromRational $ compressionRatio prop

  res <- (bfWithConf (toAutoConf evoAwareConf) emptyConf) lProp
  writeFile "LinuxRes" (show res)
  -- res <- satWith emptyConf l1Prop

 --  defaultMain
 --    [
 --      bgroup "Linux" [ mkBench "v-->v" "V1" (satWith emptyConf) l1Prop
 --                     -- , mkBench "v-->v" "V2" (satWithConf (toAutoConf d1Conf) emptyConf) bProp
 --                     -- , mkBench "v-->v" "V3" (satWithConf (toAutoConf d2Conf) emptyConf) bProp
 --                     -- , mkBench "v-->v" "V4" (satWithConf (toAutoConf d3Conf) emptyConf) bProp
 --                     -- , mkBench "v-->v" "V5" (satWithConf (toAutoConf d4Conf) emptyConf) bProp
 --                     -- , mkBench "v-->v" "V6" (satWithConf (toAutoConf d5Conf) emptyConf) bProp
 --                     -- , mkBench "v-->v" "V7" (satWithConf (toAutoConf d6Conf) emptyConf) bProp
 --                     -- , mkBench "v-->v" "V8" (satWithConf (toAutoConf d7Conf) emptyConf) bProp
 --                     -- , mkBench "v-->v" "V9" (satWithConf (toAutoConf d8Conf) emptyConf) bProp
 --                     -- , mkBench "v-->v" "V10" (satWithConf (toAutoConf d9Conf) emptyConf) bProp
 --                     -- , mkBench "v-->v" "EvolutionAware" (satWithConf (toAutoConf evoAwareConf) emptyConf) bProp

 --                  -- , mkBench "v-->v" "V1*V2"                          (satWith emptyConf) justbPropV12
 --                  -- , mkBench "v-->v" "V1*V2*V3"                       (satWith emptyConf) justbPropV123
 --                  -- , mkBench "v-->v" "V1*V2*V3*V4"                    (satWith emptyConf) justbPropV1234
 --                  -- , mkBench "v-->v" "V1*V2*V3*V4*V5"                 (satWith emptyConf) justbPropV12345
 --                  -- , mkBench "v-->v" "V1*V2*V3*V4*V5*V6"              (satWith emptyConf) justbPropV123456
 --                  -- , mkBench "v-->v" "V1*V2*V3*V4*V5*V6*V7"           (satWith emptyConf) justbPropV1234567
 --                  -- , mkBench "v-->v" "V1*V2*V3*V4*V5*V6*V7*V8"        (satWith emptyConf) justbPropV12345678
 --                  -- , mkBench "v-->v" "V1*V2*V3*V4*V5*V6*V7*V8*V9"     (satWith emptyConf) justbPropV123456789
 --                  -- , mkBench "v-->v" "V1*V2*V3*V4*V5*V6*V7*V8*V9*V10" (satWith emptyConf) bProp
 --  -- p - v
 --                  -- , mkBench "p-->v" "V1" (pOnVWithConf  Nothing) bPropV1
 --                  -- , mkBench "p-->v" "V2" (pOnVWithConf  Nothing) bPropV2
 --                  -- , mkBench "p-->v" "V3" (pOnVWithConf  Nothing) bPropV3
 --                  -- , mkBench "p-->v" "V4" (pOnVWithConf  Nothing) bPropV4
 --                  -- , mkBench "p-->v" "V5" (pOnVWithConf  Nothing) bPropV5
 --                  -- , mkBench "p-->v" "V6" (pOnVWithConf  Nothing) bPropV6
 --                  -- , mkBench "p-->v" "V7" (pOnVWithConf  Nothing) bPropV7
 --                  -- , mkBench "p-->v" "V8" (pOnVWithConf  Nothing) bPropV8
 --                  -- , mkBench "p-->v" "V9" (pOnVWithConf  Nothing) bPropV9
 --                  -- , mkBench "p-->v" "V10" (pOnVWithConf  Nothing) bPropV10
 --                  -- , mkBench "p-->v" "EvolutionAware" (pOnVWithConf (toAutoConf evoAwareConf)) bProp
 --  -- p - p
 --                  -- , mkBench "p-->p" "V1" (bfWith  emptyConf) bPropV1
 --                  -- , mkBench "p-->p" "V2" (bfWith  emptyConf) bPropV2
 --                  -- , mkBench "p-->p" "V3" (bfWith  emptyConf) bPropV3
 --                  -- , mkBench "p-->p" "V4" (bfWith  emptyConf) bPropV4
 --                  -- , mkBench "p-->p" "V5" (bfWith  emptyConf) bPropV5
 --                  -- , mkBench "p-->p" "V6" (bfWith  emptyConf) bPropV6
 --                  -- , mkBench "p-->p" "V7" (bfWith  emptyConf) bPropV7
 --                  -- , mkBench "p-->p" "V8" (bfWith  emptyConf) bPropV8
 --                  -- , mkBench "p-->p" "V9" (bfWith  emptyConf) bPropV9
 --                  -- , mkBench "p-->p" "V10" (bfWith  emptyConf) bPropV10
 --                  -- , mkBench "p-->p" "EvolutionAware" (bfWithConf (toAutoConf evoAwareConf) emptyConf) bProp
 -- -- v - p
 --                  -- , mkBench "v-->p" "V1" (bfWithConf (toAutoConf d0Conf) emptyConf) bProp
 --                  -- , mkBench "v-->p" "V2" (bfWithConf (toAutoConf d1Conf) emptyConf) bProp
 --                  -- , mkBench "v-->p" "V3" (bfWithConf (toAutoConf d2Conf) emptyConf) bProp
 --                  -- , mkBench "v-->p" "V4" (bfWithConf (toAutoConf d3Conf) emptyConf) bProp
 --                  -- , mkBench "v-->p" "V5" (bfWithConf (toAutoConf d4Conf) emptyConf) bProp
 --                  -- , mkBench "v-->p" "V6" (bfWithConf (toAutoConf d5Conf) emptyConf) bProp
 --                  -- , mkBench "v-->p" "V7" (bfWithConf (toAutoConf d6Conf) emptyConf) bProp
 --                  -- , mkBench "v-->p" "V8" (bfWithConf (toAutoConf d7Conf) emptyConf) bProp
 --                  -- , mkBench "v-->p" "V9" (bfWithConf (toAutoConf d8Conf) emptyConf) bProp
 --                  -- , mkBench "v-->p" "V10" (bfWithConf (toAutoConf d9Conf) emptyConf) bProp
 --                  -- , mkBench "v-->p" "EvolutionAware" (bfWithConf (toAutoConf evoAwareConf) emptyConf) bProp

 --                  -- , mkBench "v-->p" "V1*V2"                          (bfWith emptyConf) justbPropV12
 --                  -- , mkBench "v-->p" "V1*V2*V3"                       (bfWith emptyConf) justbPropV123
 --                  -- , mkBench "v-->p" "V1*V2*V3*V4"                    (bfWith emptyConf) justbPropV1234
 --                  -- , mkBench "v-->p" "V1*V2*V3*V4*V5"                 (bfWith emptyConf) justbPropV12345
 --                  -- , mkBench "v-->p" "V1*V2*V3*V4*V5*V6"              (bfWith emptyConf) justbPropV123456
 --                  -- , mkBench "v-->p" "V1*V2*V3*V4*V5*V6*V7"           (bfWith emptyConf) justbPropV1234567
 --                  -- , mkBench "v-->p" "V1*V2*V3*V4*V5*V6*V7*V8"        (bfWith emptyConf) justbPropV12345678
 --                  -- , mkBench "v-->p" "V1*V2*V3*V4*V5*V6*V7*V8*V9"     (bfWith emptyConf) justbPropV123456789

 --                  -- , mkBench "v-->p" "V1*V2*V3*V4*V5*V6*V7*V8*V9*V10" (bfWith emptyConf) bProp
 --                  -- , bench "VSolve:V2" . nfIO $ satWithConf (toAutoConf d1Conf) emptyConf bProp
 --                  -- , bench "VSolve:V3" . nfIO $ satWithConf (toAutoConf d2Conf) emptyConf bProp
 --                  -- , bench "VSolve:V4" . nfIO $ satWithConf (toAutoConf d3Conf) emptyConf bProp
 --                  -- , bench "VSolve:V5" . nfIO $ satWithConf (toAutoConf d4Conf) emptyConf bProp
 --                  -- , bench "VSolve:V6" . nfIO $ satWithConf (toAutoConf d5Conf) emptyConf bProp
 --                  -- , bench "VSolve:V7" . nfIO $ satWithConf (toAutoConf d6Conf) emptyConf bProp
 --                  -- , bench "VSolve:V8" . nfIO $ satWithConf (toAutoConf d7Conf) emptyConf bProp
 --                  -- , bench "VSolve:V9" . nfIO $ satWithConf (toAutoConf d8Conf) emptyConf bProp
 --                  -- , bench "VSolve:V10"  . nfIO $ satWithConf (toAutoConf d15Conf) emptyConf bProp
 --                  -- , bench "VSolve:Evo-Aware" . nfIO $ satWithConf (toAutoConf evoAwareConf) emptyConf bProp
 --                  ]
 --    ]
