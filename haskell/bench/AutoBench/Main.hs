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

critConfig = defaultConfig {resamples = 2}

-- | generate an infinite list of unique strings and take n of them dropping the
-- empty string
stringList :: Int -> [String]
stringList n = tail . take (n+1) $ concatMap (flip replicateM "abc") [0..]

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

      !sProp = (naiveEncode . nestChoices . autoToVSat) $ autoAndJoin sPs
      !bProp = (naiveEncode . nestChoices . autoToVSat) $ autoAndJoin bPs

      conjoin' = foldr' (&&&) (LitB True)
      seed = stringList 12
      seed' = drop 2 seed
      chcSeed = take 2 seed
      left  = bRef <$> take 5 seed'
      right = bRef <$> drop 5 seed'
      c :: VProp String String String
      c = OpB Not $ ChcB "AA"
          (conjoin' $ bRef <$> take 1 chcSeed)
          (conjoin' $ bRef <$> drop 1 chcSeed)
      oProp :: VProp String String String
      oProp = conjoin' $! left ++ right ++ pure c
      uoProp = conjoin' $! left ++ c:right
      badProp :: VProp String String String
      badProp = conjoin' $! c:left ++ right

  -- print $ oProp
  -- putStrLn "--------------\n"
  -- print $ uoProp
  -- print $ oProp
  -- putStrLn "--------------\n"
  putStrLn $ "UnOpt'd: " ++ show uoProp
  putStrLn $ "Opt'd: " ++ show oProp
  putStrLn $ "bad'd: " ++  show badProp
  putStrLn "--------------\n"
  putStrLn "UnOptd"
  res <- satWith emptyConf $! uoProp
  putStrLn "--------------\n"
  putStrLn "Optimized"
  res' <- satWith emptyConf $! oProp
  putStrLn "--------------\n"
  putStrLn "Bad Prop"
  res'' <- satWith emptyConf $! badProp
  putStrLn "--------------\n"
  putStrLn "Results"
  print $ length $ show res
  print $ length $ show res'
  print $ length $ show res''
  -- res' <- runIncrementalSolve bPs
  -- print sPs
  -- T.writeFile "testoutputSAT" (pack . show $ res)
  -- T.writeFile "testoutputInc" (pack . show $ res')
  -- print res
  -- let !p = prop 6000
  -- print $ length p
  -- -- res <- test 10
  -- res <- S.runSMT $ do p' <- mapM S.sBool p
  --                      SC.query $! test' p'
  -- putStrLn "Running Good:\n"
  -- goodRes <- testS goodS 1000

  -- defaultMainWith critConfig
  --   [
  --   bgroup "vsat" [ -- bench "small file:NoOpts"  . nfIO $ satWith emptyConf sProp
  --                  --, bench "small file:DefOpts" . nfIO $ satWith defConf   sProp
  --                   bench "unOpt" . nfIO $ satWith emptyConf uoProp
  --                 , bench "Opt" . nfIO $ satWith emptyConf oProp
  --                 , bench "BadOpt" . nfIO $ satWith emptyConf badProp
  --                 -- , bench "def:unOpt" . nfIO $ satWith defConf uoProp
  --                 -- , bench "def:Opt" . nfIO $ satWith defConf oProp
  --                 -- , bench "def:BadOpt" . nfIO $ satWith defConf badProp
  --                 -- , bench "large file:NoOpts"  . nfIO $ satWith emptyConf bProp
  --                 -- , bench "large file:DefOpts" . nfIO $ satWith defConf   bProp
  --                -- bench "large file" . whnfIO $ runIncrementalSolve bPs
  --                 ]
  --   ]
