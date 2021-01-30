module Main where

import           Control.Arrow           (first, second)
import           Control.DeepSeq         (force)
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
analysisToVariational a = fm &&&
                          nM &&&
                          lexProblems &&&
                          parseProblems &&&
                          tcProblems
  where fm            = featureModel a
        handleEmpty a = if null a then [true] else a
        nM            = prop Nothing $ handleEmpty $ noMode a
        nmLength      = length $ noMode a
        lexLength     = nmLength + (length $ lexing a)
        tcLength      = lexLength + (length $ typeChecking a)
        lexProblems   = (\p -> bChc "Lexing" p true)       $ prop (Just nmLength)  $ handleEmpty $ lexing       a
        parseProblems = (\p -> bChc "Parsing" p true)      $ prop (Just lexLength) $ handleEmpty $ parsing      a
        tcProblems    = (\p -> bChc "TypeChecking" p true) $ prop (Just tcLength)  $ handleEmpty $ typeChecking a


constructVariational :: [Analysis Readable Readable] -> ReadableProp T.Text
constructVariational = conjoin . fmap analysisToVariational

onlyLex       = bRef "Lexing" &&& bnot (bRef "Parsing") &&& bnot (bRef "TypeChecking")
onlyParse     = bnot (bRef "Lexing") &&& bRef "Parsing" &&& bnot (bRef "TypeChecking")
onlyTypeCheck = bnot (bRef "Lexing") &&& bnot (bRef "Parsing") &&& bRef "TypeChecking"

vc :: Maybe (DimProp T.Text)
vc = toDimProp vc'

vc' = onlyLex |||
     (onlyLex &&& onlyParse) |||
     (onlyLex &&& onlyParse &&& onlyTypeCheck)

-- run with stack bench --profile vsat:busybox --benchmark-arguments='+RTS -S -RTS --output timings.html'
main = do
  let benches :: ReadableSMTConf T.Text -> [Analysis Readable Readable] -> [Benchmark]
      benches solverConf as =
        [ mkBench "Variational" "BusyBox" vc' (satWith solverConf) (constructVariational as)
        , bench "Incremental/BusyBox" (nfIO (constructIncremental as))
        , mkBench' "BruteForce"  "BusyBox" (bfWith solverConf) (constructBF as) -- notice no vc for bf because the dimensions are synthetic
        ]

  !ps <- getProblems
  let !bf = force $! constructBF ps

  putStrLn $!  "BF Made!!!" ++ (show $ length ps)
  -- defaultMain [ bgroup "Z3" (benches z3DefConf ps)
  --             ]

  putStrLn "Solving!!"
  -- results <- constructIncremental ps
  -- results <- satWithConf vc minConf (constructVariational ps)
  -- results <- satWithConf vc minConf (constructIncremental ps)
  -- putStrLn $ "Results !!" ++ show results
  bfWith z3DefConfOnlySat bf
