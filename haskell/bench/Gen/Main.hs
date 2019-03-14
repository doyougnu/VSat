module Main where

import           Control.Arrow           (first, second)
import           Criterion.Main
import           Criterion.Main.Options
import           Criterion.Types         (Config (..))
import           Data.Aeson              (decodeStrict)
import           Control.Monad           (replicateM, foldM, liftM2)
import           Test.Tasty.QuickCheck
import           Data.Bifunctor          (bimap)
import           Data.Bitraversable      (bimapM)
import qualified Data.ByteString         as BS (readFile)
import           Data.Either             (lefts, rights)
import           Data.Foldable           (foldr',foldl')
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
import           VProp.Gen
import           VProp.SBV               (toPredicate)
import           VProp.Types

import           CaseStudy.Auto.Run

critConfig = defaultConfig {resamples = 12}

-- | generate an infinite list of unique strings and take n of them dropping the
-- empty string
stringList :: Int -> [String]
stringList n = tail . take (n+1) $ concatMap (flip replicateM "abc") [0..]

-- run with stack bench --profile vsat:auto --benchmark-arguments='+RTS -S -RTS --output timings.html'
main = do
  let conjoin' = foldr' (&&&) (LitB True)
      genIt n = genBoolProp (genVPropAtSize n genReadable)
      m = 1000
      n = 1000
      seed' = stringList m
      chcSeedL = show <$> [0..n]
      chcSeedR = show <$> take n [n+1..]
      left  = bRef <$> take (m `div` 2) seed'
      right = bRef <$> drop (m `div` 2) seed'
      a :: VProp String String String
      a = ChcB "AA"
          (conjoin' $! bRef <$> chcSeedL)
          (conjoin' $! bRef <$> chcSeedR)

      b :: VProp String String String
      b = ChcB "BB"
          (conjoin' $! bRef <$> fmap (++"a") chcSeedL)
          (conjoin' $! bRef <$> fmap (++"b") chcSeedR)

      c :: VProp String String String
      c = ChcB "CC"
          (conjoin' $! bRef <$> fmap (++"c") chcSeedL)
          (conjoin' $! bRef <$> fmap (++"d") chcSeedR)

      d :: VProp String String String
      d = ChcB "DD"
          (conjoin' $! bRef <$> fmap (++"e") chcSeedL)
          (conjoin' $! bRef <$> fmap (++"f") chcSeedR)

      oProp :: VProp String String String
      oProp = conjoin'   $! left ++ right ++ [a,b,c,d]
      uoProp = conjoin'  $! left ++ a:b:c:d:right
      badProp :: VProp String String String
      badProp = conjoin' $! a:b:c:d:left ++ right

  -- print oProp
  -- res <- satWith debugConf oProp
  -- -- res' <- runIncrementalSolve $! breakOnAnd $ vPropToAuto oProp
  -- putStrLn "--------------\n"
  -- putStrLn "Result"
  -- print $ res
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
  -- print $ res''
  -- ps <- mapM (generate . genIt) [1..55]
  -- mapM_ (putStrLn . show) ps

  defaultMainWith critConfig
    [
    bgroup "vsat" [ bench "unOpt" . nfIO $ satWith emptyConf uoProp
                  , bench "Opt" . nfIO $ satWith emptyConf oProp
                  , bench "BadOpt" . nfIO $ satWith emptyConf badProp
                  , bench "def:unOpt" . nfIO $ satWith defConf uoProp
                  , bench "def:Opt" . nfIO $ satWith defConf oProp
                  , bench "def:BadOpt" . nfIO $ satWith defConf badProp
                  ]
    ]
