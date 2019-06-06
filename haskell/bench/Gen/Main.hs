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
import           Data.Foldable           (foldr',foldl')
import           Data.List               (sort,splitAt,intersperse,foldl1')
import           Data.Map                (size, Map, toList)
import qualified Data.SBV                as S
import qualified Data.SBV.Control        as SC
import qualified Data.SBV.Internals      as SI
import           Data.Text               (pack, unpack,Text,cons,append,singleton)
import qualified Data.Text.IO            as T (writeFile)
import           System.IO
import           Text.Megaparsec         (parse)
import           Text.Show.Unicode          (ushow)

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
import           VProp.Boolean

import           CaseStudy.Auto.Run

default (Text)
-- | generate an infinite list of unique strings and take n of them dropping the
-- empty string
stringList :: Int -> [Text]
stringList n = fmap pack . tail . take (n+1) $
               concatMap (flip replicateM "abc") [0..]


conjoin' :: [VProp Text Text Text] -> VProp Text Text Text
conjoin' = foldl1' (&&&)

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

run !desc !conf !f = bench desc $! nfIO (satWith conf f)

-- | keep a desc so that you can add to it later
runEmpty !descriptor !f !nChc !vSize !nPln = run desc emptyConf prop
  where !desc' = [ "Chc",show nChc
                 , "VariantSize",show vSize
                 , "numPlain", show nPln
                 , "Compression", show ratio
                 ]
        !alg = ["VSolve","Empty"]
        !desc = mconcat $ intersperse "/" $ alg ++ desc' ++ descriptor
        !prop = (f nChc vSize nPln)
        ratio :: Double
        !ratio = fromRational $ compressionRatio prop

runDef !descriptor !f !nChc !vSize !nPln = run desc emptyConf prop
  where !desc' = [ "Chc",show nChc
                 , "VariantSize",show vSize
                 , "numPlain", show nPln
                 , "Compression", show ratio
                 ]
        !alg = ["VSolve","Def"]
        !desc = mconcat $ intersperse "/" $ alg ++ desc' ++ descriptor
        !prop = applyOpts defConf $! f nChc vSize nPln
        ratio :: Double
        !ratio = fromRational $ compressionRatio prop

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
      dimConf' :: VProp Text String String
      dimConf' = ((bRef "aa") &&& bRef "bb") ||| ((bRef "aa") &&& bnot (bRef "bb"))
      dimConf = toDimProp dimConf'
      -- nC = 5
      -- nV = 20
      -- nP = 200
      nC = 4
      nV = 10
      nP = 100

      !propM = midGen nC nV nP
      -- !propL = leftGen nC nV nP
      !propR = rightGen nC nV nP
      !optdPropMSorted = pSort propM
      !optdPropMAssoc = associativeLeft $ pSort propM
      -- !optdPropL = applyOpts defConf propL
      n = [10,100]
      -- v = [10,200]
      m = [2,4]

      -- propsER = runEmptyRight <$> m <*> n <*> n
      -- propsEL = runEmptyLeft <$> m <*> n <*> n
      propsEM = runEmptyMid <$> m <*> n <*> n
      -- propsOER = runDefRight <$> m <*> n <*> n
      -- propsOEL = runDefLeft <$> m <*> n <*> n
      propsOEM = runDefMid <$> m <*> n <*> n
      -- !propsNaive =  concat $ zipWith3 (\x y z -> [x,y,z]) propsER propsEL propsEM
      -- !propsOptd =  concat $ zipWith3 (\x y z -> [x,y,z]) propsOER propsOEL propsOEM
      -- !props = concat $ zipWith (\x y -> [x,y]) propsNaive propsOptd
      interleaveProps x y  = concat $ zipWith (\x y -> [x,y]) x y
      props = interleaveProps propsEM propsOEM
      -- testProp = (bChc "AA" (bRef "a" &&& bRef "d") (bRef "b")) &&& (bChc "BB" (bRef "a" &&& bRef "d") (bRef "a"))
      testProp :: ReadableProp Text
      -- testProp = (bChc ("AA" :: Text) (bRef "a") (bRef "b"))  &&& (bnot (bRef "a") &&& (bRef "c")) &&& (bChc "DD" (bnot (bRef "b")) (bRef "f"))
      -- testProp = (bChc ("AA" :: Text) (bChc "DD" (bRef "a") (bRef "b")) (bRef "b"))  &&& (bnot (bRef "a") &&& (bRef "c")) &&& (bChc "DD" (bnot (bRef "b")) (bRef "f"))
      testProp = (bChc ("AA" :: Text) (bChc "DD" (bRef "a") (bRef "b")) (bRef "b")) <=> ((bRef "c") &&& (bnot (bRef "c")))
      testDimConf = toDimProp $ (bRef "AA" &&& bRef "BB")



  putStrLn $ show testProp
  -- res <- satWith emptyConf testProp
  res <- satWithConf (Just $ testDimConf) emptyConf testProp
  putStrLn $ ushow res
  -- models <- deriveModels res

  -- putStrLn "Models: "
  -- putStrLn $ show models
  -- let resMap = getResMap res
  --     m' = first pack <$> m
  -- let valLists = deriveValues res <$> models

  -- let lits = (flip substitute testProp . Data.Map.toList <$> valLists)
  --     res = selectVariant <$> models <*> lits

  -- putStrLn $ "valLists: " ++  show valLists
  -- putStrLn $ "Literals: " ++ show lits
  -- putStrLn $ "props " ++ show res
  -- putStrLn ""
  -- putStrLn $ "[Mid]: " ++ show propM
  -- -- putStrLn $ "[MidS]: " ++ show (propR < propM)
  -- putStrLn ""
  -- putStrLn $ "[Sorted]: " ++ (show optdPropMSorted)
  -- putStrLn $ "[Ass]: " ++ (show optdPropMAssoc)
  -- putStrLn ""
  -- putStrLn $ "[RIGHT]: " ++ show propR

  -- putStrLn $ "----------"
  -- putStrLn $ "Empty Ratios: "
  -- putStrLn $ show (compressionRatio propL :: Rational)
  -- putStrLn $ show (compressionRatio propM :: Rational)
  -- putStrLn $ show (compressionRatio propR :: Rational)
  -- putStrLn $ "Def Ratios: "
  -- putStrLn $ show (compressionRatio optdPropL :: Rational)
  -- putStrLn $ show (compressionRatio optdPropM :: Rational)
  -- putStrLn $ show (compressionRatio optdPropR :: Rational)

  -- defaultMain
  --   [
  --   bgroup "vsat" -- props
  --     [ -- bench "Empty:propL"  $ nfIO $! satWith emptyConf propL
  --     bench "Empty:propM" $ nfIO  $! satWith emptyConf propM
  --     , bench "Empty:propM:Sorted" $ nfIO  $! satWith emptyConf optdPropMSorted
  --     , bench "Empty:propM:Assoc" $ nfIO  $! satWith emptyConf optdPropMAssoc
  --     -- , bench "Empty:propR"  $ nfIO $! satWith emptyConf propR
  --     -- , bench "Def:propL"  $ nfIO $! satWith emptyConf propL
  --     -- , bench "Def:propM" $ nfIO  $! satWith emptyConf propM
  --     , bench "Def:propR"  $ nfIO $! satWith emptyConf propR
  --     ]
  --   ]
