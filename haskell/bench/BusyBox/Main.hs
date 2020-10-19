module Main where

import           Control.Arrow           (first, second)
import           Gauge
import           Data.Aeson              (decodeStrict, encodeFile)
import           Control.Monad           (replicateM, foldM, liftM2)
import           Data.Bifunctor          (bimap)
import           Data.Bitraversable      (bimapM)
import           Data.Either             (lefts, rights)
import           Data.Foldable           (foldr')
import qualified Data.List               as L
import           Data.Map                (size, Map, toList)
import qualified Data.SBV                as S
import qualified Data.SBV.Control        as SC
import qualified Data.SBV.Internals      as SI
import qualified Data.Set                as Set
import qualified Data.Text               as T
import qualified Data.Text.IO            as TIO
import           System.IO
import           Text.Megaparsec         (parse)
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

import           Core
import           BruteForce
import           BusyBox
import           Incremental

dataFile :: FilePath
-- dataFile = "bench/BusyBox/SAT_uniq_sorted.txt"
dataFile = "bench/BusyBox/SAT_problems.txt"

-- | I wonder if having an alternative with all the queries is better than
-- having a list of choices with all the same alternatives...
analysisToVariational :: Analysis Readable Readable -> ReadableProp T.Text
analysisToVariational a = fm &&& nM &&& lexProblems &&& parseProblems &&& tcProblems
  where fm            = featureModel a
        nM            = conjoin $ noMode a
        lexProblems   = conjoin $ (\p -> bChc "Lexing" p true)       <$> lexing       a
        parseProblems = conjoin $ (\p -> bChc "Parsing" p true)      <$> parsing      a
        tcProblems    = conjoin $ (\p -> bChc "TypeChecking" p true) <$> typeChecking a


constructVariational :: [Analysis Readable Readable] -> ReadableProp T.Text
constructVariational = conjoin . fmap analysisToVariational

onlyLex = bRef "Lexing" &&& bnot (bRef "Parsing") &&& bnot (bRef "TypeChecking")
onlyParse = bnot (bRef "Lexing") &&& bRef "Parsing" &&& bnot (bRef "TypeChecking")
onlyTypeCheck = bnot (bRef "Lexing") &&& bnot (bRef "Parsing") &&& bRef "TypeChecking"

vc = onlyLex |||
     (onlyLex &&& onlyParse) |||
     (onlyLex &&& onlyParse &&& onlyTypeCheck)


-- run with stack bench --profile vsat:busybox --benchmark-arguments='+RTS -S -RTS --output timings.html'
main = do
  let benches :: ReadableSMTConf T.Text -> [Analysis Readable Readable] -> [Benchmark]
      benches solverConf as =
        [ mkBench "BruteForce"  "BusyBox" vc (bfWith solverConf) (constructBF as)
        , mkBench "Variational" "BusyBox" vc (satWith solverConf) (constructVariational as)
        , bench "Incremental/BusyBox" (nfIO (constructIncremental as))
        ]

  ps <- getProblems
  defaultMain
    [ bgroup "Z3" (benches z3DefConf ps)
    ]
  -- satWith z3DefConf (propOpts problems)
  -- dir >>= print
  -- results <- constructIncremental ps
  -- print results


  -- print $ pivotList . prop $ ts
  -- print $ dimensions $ prop ts
  -- print $ prop ts
  -- print $ Set.size $ dimensions $ prop problems
  -- print $ length problems
  -- print $ prop ts
  -- (satWith z3DefConf) bProp >>= encodeFile "data/fin_vmodel.json"
