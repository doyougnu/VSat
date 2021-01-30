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


-- | construct a brute force analysis for an analysis. Check if there is a
-- feature model, if so then prepend it to all the queries, then we wrap all
-- queries into unique choices. The brute force algorithm in vsat for the splc
-- paper will unwind all the choices and perform then interface to the solver
-- for us
analysisToBF :: Analysis Readable Readable -> [ReadableProp T.Text]
analysisToBF (getAnalysis -> a) = problems
  where
    queries = M.elems a
    problems = case M.lookup FeatureModel a of
                 Nothing -> mconcat queries
                 Just (f:_)  -> concatMap (fmap ((&&&) f)) queries

constructBF :: [Analysis Readable Readable] -> ReadableProp T.Text
constructBF = prop Nothing . concatMap analysisToBF
