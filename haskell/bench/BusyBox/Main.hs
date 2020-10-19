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

dataFile :: FilePath
-- dataFile = "bench/BusyBox/SAT_uniq_sorted.txt"
dataFile = "bench/BusyBox/SAT_problems.txt"

-- run with stack bench --profile vsat:busybox --benchmark-arguments='+RTS -S -RTS --output timings.html'
main = do
  -- txtProblems <- T.lines <$> TIO.readFile dataFile
  -- let problems' = parse langParser "" <$> txtProblems
  --     !problems = rights problems'

  --     base :: Integer -> Integer -> Integer
  --     base b = ceiling . logBase (fromInteger b) . fromInteger

  --     hole :: ReadableProp T.Text
  --     hole = bRef "__"


  --     prop :: [ReadableProp T.Text] -> ReadableProp T.Text
  --     prop xs = outer 0 xs
  --       where
  --         outer i [x] = x
  --         outer i xs  = outer (succ i) (inner (T.pack $ show i) xs)


  --         inner :: T.Text -> [ReadableProp T.Text] -> [ReadableProp T.Text]
  --         inner _ [] = []
  --         inner _ [x] = [x]
  --         inner d (x:y:xs) = ChcB (Dim d) x y : inner d xs

  --     propOpts :: [ReadableProp T.Text] -> ReadableProp T.Text
  --     propOpts = atomize . outer 0
  --       where
  --         outer _ [x] = x
  --         outer i ys  = outer (succ i) (atomize <$> inner (T.pack $ show i) ys)


  --         inner :: T.Text -> [ReadableProp T.Text] -> [ReadableProp T.Text]
  --         inner _ [] = []
  --         inner _ [x] = [x]
  --         inner d (x:y:ys) = ChcB (Dim d) x y : inner d ys

  --     benches :: ReadableSMTConf T.Text -> [Benchmark]
  --     benches solverConf =
  --       [ mkBench' "Variational" "BusyBox.Uniques.Opts" (satWith solverConf) (propOpts problems)
  --       , mkBench' "Variational" "BusyBox.Uniques" (satWith solverConf) (prop problems)
  --       , mkBench' "BruteForce"  "BusyBox.Uniques" (bfWith  solverConf) (prop problems)
  --       ]

  -- defaultMain
  --   [ bgroup "Z3" (benches z3DefConf)
  --   ]
  -- satWith z3DefConf (propOpts problems)
  -- dir >>= print
  test

  -- print $ pivotList . prop $ ts
  -- print $ dimensions $ prop ts
  -- print $ prop ts
  -- print $ Set.size $ dimensions $ prop problems
  -- print $ length problems
  -- print $ prop ts
  -- (satWith z3DefConf) bProp >>= encodeFile "data/fin_vmodel.json"
