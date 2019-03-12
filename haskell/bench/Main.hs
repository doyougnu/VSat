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
import           Data.List               (sort)
import           Data.Map                (size, Map)
import qualified Data.SBV                as S
import qualified Data.SBV.Control        as SC
import qualified Data.SBV.Internals      as SI
import           Data.Text               (pack, unpack)
import qualified Data.Text.IO            as T (writeFile)
import           System.IO
import           Text.Megaparsec         (parse)

import           Api
import           CaseStudy.Auto.Auto
import           CaseStudy.Auto.Parser   (langParser)
import           CaseStudy.Auto.Run
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

critConfig = defaultConfig {resamples = 12}

-- | generate an infinite list of unique strings and take n of them dropping the
-- empty string
stringList :: Int -> [String]
stringList n = tail . take (n+1) $ concatMap (flip replicateM "abc") [0..]

-- run with stack bench --profile vsat:auto --benchmark-arguments='+RTS -S -RTS --output timings.html'
main = do
  let conjoin' = foldr' (&&&) (LitB True)
      seed = stringList 12000
      seed' = drop 2000 seed
      chcSeed = take 2000 seed
      left  = bRef <$> take 5000 seed'
      right = bRef <$> drop 5000 seed'
      a :: VProp String String String
      a = OpB Not $ ChcB "AA"
          (conjoin' $ bRef <$> take 1000 chcSeed)
          (conjoin' $ bRef <$> drop 1000 chcSeed)

      b :: VProp String String String
      b = OpB Not $ ChcB "BB"
          (conjoin' $ bRef <$> take 1000 chcSeed)
          (conjoin' $ bRef <$> drop 1000 chcSeed)

      c :: VProp String String String
      c = OpB Not $ ChcB "CC"
          (conjoin' $ bRef <$> take 1000 chcSeed)
          (conjoin' $ bRef <$> drop 1000 chcSeed)

      d :: VProp String String String
      d = OpB Not $ ChcB "DD"
          (conjoin' $ bRef <$> take 1000 chcSeed)
          (conjoin' $ bRef <$> drop 1000 chcSeed)

      oProp :: VProp String String String
      oProp = conjoin' $! left ++ right ++ [a,b,d,c]
      uoProp = conjoin' $! left ++ a:b:d:c:right
      badProp :: VProp String String String
      badProp = conjoin' $! a:b:c:d:left ++ right

  -- print $ oProp
  -- putStrLn "--------------\n"
  -- print $ uoProp
  -- print $ oProp
  -- putStrLn "--------------\n"
  -- putStrLn $ "UnOpt'd: " ++ show uoProp
  -- putStrLn $ "Opt'd: " ++ show oProp
  -- putStrLn $ "bad'd: " ++  show badProp
  -- putStrLn "--------------\n"
  -- putStrLn "UnOptd"
  -- res <- satWith emptyConf $! uoProp
  -- putStrLn "--------------\n"
  -- putStrLn "Optimized"
  -- res' <- satWith emptyConf $! oProp
  -- putStrLn "--------------\n"
  -- putStrLn "Bad Prop"
  -- res'' <- satWith emptyConf $! badProp
  -- putStrLn "--------------\n"
  -- putStrLn "Results"
  -- print $ length $ show res
  -- print $ length $ show res'
  -- print $ length $ show res''

  defaultMainWith critConfig
    [
    bgroup "vsat" [ -- bench "small file:NoOpts"  . nfIO $ satWith emptyConf sProp
                   --, bench "small file:DefOpts" . nfIO $ satWith defConf   sProp
                    bench "unOpt" . nfIO $ satWith emptyConf uoProp
                  , bench "Opt" . nfIO $ satWith emptyConf oProp
                  , bench "BadOpt" . nfIO $ satWith emptyConf badProp
                  -- , bench "def:unOpt" . nfIO $ satWith defConf uoProp
                  -- , bench "def:Opt" . nfIO $ satWith defConf oProp
                  -- , bench "def:BadOpt" . nfIO $ satWith defConf badProp
                  -- , bench "large file:NoOpts"  . nfIO $ satWith emptyConf bProp
                  -- , bench "large file:DefOpts" . nfIO $ satWith defConf   bProp
                 -- bench "large file" . whnfIO $ runIncrementalSolve bPs
                  ]
    ]
