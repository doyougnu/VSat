{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
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
import           Data.List               (sort,splitAt,intersperse)
import           Data.Map                (size, Map)
import qualified Data.SBV                as S
import qualified Data.SBV.Control        as SC
import qualified Data.SBV.Internals      as SI
import           Data.Text               (pack, unpack,Text,cons,append,singleton)
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

default (Text)
-- | generate an infinite list of unique strings and take n of them dropping the
-- empty string
stringList :: Int -> [Text]
stringList n = fmap pack . tail . take (n+1) $
               concatMap (flip replicateM "abc") [0..]

conjoin' :: [VProp Text Text Text] -> VProp Text Text Text
conjoin' = foldl' (&&&) (LitB True)

genChcList :: Int -> Int -> [VProp Text Text Text]
genChcList nChc variantSize = chc's
  where
    dims :: [Text]
    dims = take nChc
           [ pack $ x:x:[]
           | x <- ['a'..]
           ]
    chc's = do d <- dims
               let
                 leftVariant  = (pack . show) <$> [0..variantSize]
                 rightVariant =  take variantSize $
                                   (pack . show) <$> [variantSize+1..]
               return $ bChc d
                 (conjoin' $ bRef . append d <$> leftVariant)
                 (conjoin' $ bRef . append d <$> rightVariant)

genChc :: Int -> Int -> VProp Text Text Text
genChc = (conjoin' .) . genChcList

genPlain :: Int -> VProp Text Text Text
genPlain = conjoin' . genPlainList

genPlainList :: Int -> [VProp Text Text Text]
genPlainList = fmap bRef . stringList

insertAt :: Int -> [VProp Text Text Text] ->
            [VProp Text Text Text] -> VProp Text Text Text
insertAt n insert seed = conjoin' $ left ++ insert ++ right
  where (left, right) = splitAt n $ seed


rightGen nChc vSize nPln = genPlain nPln &&& genChc nChc vSize

midGen  nChc vSize nPln = insertAt
                          (nPln `div` 2)
                          (genChcList nChc vSize)
                          (genPlainList nPln)
leftGen nChc vSize nPln = genChc nChc vSize &&& genPlain nPln

run desc conf f =
  bench desc $! nfIO (satWith conf f)

-- | keep a desc so that you can add to it later
runEmpty descriptor f nChc vSize nPln = run desc emptyConf (f nChc vSize nPln)
  where desc' = [ "Chc",show nChc
                , "VariantSize",show vSize
                , "numPlain", show nPln
                ]
        alg = ["VSolve","Empty"]
        desc = mconcat $ intersperse "/" $ alg ++ desc' ++ descriptor

runDef descriptor f nChc vSize nPln = run desc defConf (f nChc vSize nPln)
  where desc' = [ "Chc",show nChc
                , "VariantSize",show vSize
                , "numPlain", show nPln
                ]
        alg = ["VSolve","Def"]
        desc = mconcat $ intersperse "/" $ alg ++ desc' ++ descriptor

runEmptyLeft = runEmpty ["LeftGen"] leftGen
runEmptyMid = runEmpty ["MidGen"] midGen
runEmptyRight = runEmpty ["RightGen"] rightGen

runDefLeft = runDef ["LeftGen"] leftGen
runDefMid = runDef ["MidGen"] midGen

runDefRight :: Int -> Int -> Int -> Benchmark
runDefRight = runDef ["RightGen"] rightGen

-- run with stack bench --profile vsat:auto --benchmark-arguments='+RTS -S -RTS --output timings.html'
main = do
  let
      runInc = runIncrementalSolve . breakOnAnd . vPropToAuto
      genIt n = genBoolProp (genVPropAtSize n genReadable)
      n = [1,300,600,900,1200]
      m = [0,2,4]
      !propsER = runEmptyRight <$> m <*> n <*> n
      !propsEL = runEmptyLeft <$> m <*> n <*> n
      !propsDL = runDefLeft <$> m <*> n <*> n
      !props = propsER ++ propsEL ++ propsDL

  defaultMain
    [
    bgroup "vsat" props
    ]
  -- print $ length $ show r
  -- print $ length $ show r'
  return ()
