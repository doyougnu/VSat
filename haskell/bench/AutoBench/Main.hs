import           Control.Arrow           (first, second)
-- import           Criterion.Main
-- import           Criterion.Main.Options
-- import           Criterion.Types         (Config (..))
import           Data.Aeson              (decodeStrict)
import           Control.Monad           (replicateM, foldM)
import           Data.Bifunctor          (bimap)
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
import           Config                  (defConf, emptyConf)
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

-- critConfig = defaultConfig {resamples = 1}

-- | generate an infinite list of unique strings and take n of them dropping the
-- empty string
stringList :: Int -> [String]
stringList n = tail . take (n+1) $ concatMap (flip replicateM "abc") [0..]

-- | the test running, takes a computation that runs in the query monad and an
-- int that dictates size of the list of sbools
test :: ([S.SBool] -> SC.Query (Map String Bool)) -> Int -> IO (Map String Bool)
test f n = S.runSMT $
           do
             prop' <- S.sBools $! stringList n
             SC.query $ f prop'

-- | I fold over the string of SBools here constraining at each accumulation,
-- this seems to blow up the internal cache severely leading to about 95% GC
-- time
bad :: [S.SBool] -> SC.Query (Map String Bool)
bad prop' = do b <- foldM (helper) S.sTrue prop'
               S.constrain b
               fmap (fmap SI.cvToBool) $ S.getModelDictionary <$> SC.getSMTResult
  -- | combine the current sbool with the accumulated sbool, constrain the
  -- two and then return the accumulated result
  where helper x acc = do let b = x S..&& acc
                          S.constrain b
                          return b

-- | identical to the bad version but I do not constrain for each accumulation
good :: [S.SBool] -> SC.Query (Map String Bool)
good prop' = do b <- foldM (helper) S.sTrue prop'
                S.constrain b
                fmap (fmap SI.cvToBool) $ S.getModelDictionary <$> SC.getSMTResult
  -- | this helper is equivalent to just foldr' (S.&&&)
  where helper x acc = do let b = x S..&& acc
                          return b


-- | the test runner, this time f returns a predicate and we have no query
-- | operations whatsoever
testS :: ([S.SBool] -> S.Predicate) -> Int -> IO S.SatResult
testS f n = S.sat $ (S.sBools $! stringList n) >>= f


-- | I fold over the string of SBools here constraining at each accumulation,
-- this seems to blow up the internal cache severely leading to about 95% GC
-- time
badS :: [S.SBool] -> S.Predicate
badS prop' = do b <- foldM (helper) S.sTrue prop'
                S.constrain b
                return b
  -- | combine the current sbool with the accumulated sbool, constrain the
  -- two and then return the accumulated result
  where helper x acc = do let b = x S..&& acc
                          S.constrain b
                          return b

-- | identical to the bad version but I do not constrain for each accumulation
goodS :: [S.SBool] -> S.Predicate
goodS prop' = do b <- foldM (helper) S.sTrue prop'
                 S.constrain b
                 return b
  -- | this helper is equivalent to just foldr' (S.&&&)
  where helper x acc = do let b = x S..&& acc
                          return b

-- run with stack bench --profile vsat:auto --benchmark-arguments='+RTS -S -RTS --output timings.html'
main = do
  -- readfile is strict
  -- sJsn <- BS.readFile smAutoFile
  -- bJsn <- BS.readFile autoFileBool
  -- let (Just sAuto) = decodeStrict sJsn :: Maybe Auto
  -- let (Just bAuto) = decodeStrict bJsn :: Maybe Auto
  --     !sCs = constraints sAuto -- looks like 4298/4299 are the culprits
  --     !bCs = constraints bAuto
  --     sPs' = parse langParser "" <$> sCs
  --     sPs = rights sPs'

  --     bPs' = parse langParser "" <$> bCs
  --     bPs = rights bPs'

  --     !sProp = (naiveEncode . nestChoices . autoToVSat) $ autoAndJoin sPs
  --     !bProp = (naiveEncode . nestChoices . autoToVSat) $ autoAndJoin (take 325 bPs)

  -- res <- satWith emptyConf $! bProp
  -- res' <- runIncrementalSolve bPs
  -- print sPs
  -- T.writeFile "testoutputSAT" (pack . show $ res)
  -- T.writeFile "testoutputInc" (pack . show $ res')
  -- print res'
  -- let !p = prop 6000
  -- print $ length p
  -- -- res <- test 10
  -- res <- S.runSMT $ do p' <- mapM S.sBool p
  --                      SC.query $! test' p'
  -- putStrLn "Running Good:\n"
  -- goodRes <- testS goodS 1000

  putStrLn "Running Bad:\n"
  badRes <- testS badS 1000

  -- just ensuring evaluation
  -- writeFile "goodRes" (show goodRes)
  writeFile "badres" (show badRes)
  -- print $ size res
  -- defaultMainWith critConfig
  --   [
  --   bgroup "vsat" [ -- bench "small file" . nfIO $ satWith emptyConf sProp
  --                 -- bench "large file" . nfIO $ satWith emptyConf bProp
  --                 -- bench "large file" . whnfIO $ runIncrementalSolve bPs
  --                 -- bench "2" . nfIO $ test 20
  --                   bench "good: 10"   . nfIO $ test good 10
  --                 , bench "good: 100"  . nfIO $ test good 100
  --                 , bench "good: 1000" . nfIO $ test good 1000
  --                 , bench "bad: 10"    . nfIO $ test bad 10
  --                 , bench "bad: 100"   . nfIO $ test bad 100
  --                 , bench "bad: 1000"  . nfIO $ test bad 1000
  --                 -- , bench "100" . nfIO $ test 100
  --                 -- , bench "1000" . nfIO $ test 1000
  --                 -- , bench "10000" . nfIO $ test 10000
  --                 -- , bench "20000" . nfIO $ test 20000
  --                 ]
  --   ]
