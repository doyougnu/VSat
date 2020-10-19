module BruteForce where


import qualified Data.Map.Strict         as M

import           Gauge
import           Data.Foldable           (foldr')
import qualified Data.List               as L
import           Data.Map                (size, Map, toList)
import qualified Data.SBV                as S
import qualified Data.SBV.Control        as SC
import qualified Data.SBV.Internals      as SI
import qualified Data.Set                as Set
import qualified Data.Text               as T
import qualified Data.Text.IO            as TIO
import Numeric
import Data.Char (intToDigit)


import           Api
import           CaseStudy.Auto.Auto
import           CaseStudy.BusyBox.Parser (langParser)
import           Config
import           Opts
import           Json
import           Run                     (runAD, runBF)
import           Result
import           Utils
import           VProp.Core
import           VProp.SBV               (toPredicate)
import           VProp.Types

import Core
import BusyBox


base :: Integer -> Integer -> Integer
base b = ceiling . logBase (fromInteger b) . fromInteger

hole :: ReadableProp T.Text
hole = bRef "__"


prop :: [ReadableProp T.Text] -> ReadableProp T.Text
prop xs = outer 0 xs
  where
    outer i [x] = x
    outer i xs  = outer (succ i) (inner (T.pack $ show i) xs)


    inner :: T.Text -> [ReadableProp T.Text] -> [ReadableProp T.Text]
    inner _ [] = []
    inner _ [x] = [x]
    inner d (x:y:xs) = ChcB (Dim d) x y : inner d xs

propOpts :: [ReadableProp T.Text] -> ReadableProp T.Text
propOpts = atomize . outer 0
  where
    outer _ [x] = x
    outer i ys  = outer (succ i) (atomize <$> inner (T.pack $ show i) ys)


    inner :: T.Text -> [ReadableProp T.Text] -> [ReadableProp T.Text]
    inner _ [] = []
    inner _ [x] = [x]
    inner d (x:y:ys) = ChcB (Dim d) x y : inner d ys

-- | construct a brute force analysis for an analysis. Check if there is a
-- feature model, if so then prepend it to all the queries
analysisToBF :: Analysis -> [ReadableProp T.Text]
analysisToBF (getAnalysis -> a) = problems
  where
    queries = M.elems a
    problems = case M.lookup FeatureModel a of
                 Nothing -> mconcat queries
                 Just (f:_)  -> concatMap (fmap ((&&&) f)) queries

constructBF :: [Analysis] -> ReadableProp T.Text
constructBF = prop . concatMap analysisToBF
