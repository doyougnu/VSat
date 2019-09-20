module Core where

import           Control.Arrow           (first, second)
import           Criterion.Main
import           Criterion.Main.Options
import           Criterion.Types         (Config (..))
import           Control.DeepSeq
import           Data.Aeson              (decodeStrict)
import           Control.Monad           (replicateM, foldM, liftM2)
import           Data.Bifunctor          (bimap)
import           Data.Bitraversable      (bimapM)
import qualified Data.ByteString         as BS (readFile)
import           Data.Either             (lefts, rights)
import           Data.Foldable           (foldr')
import           Data.List               (sort,splitAt,intersperse,foldl1',delete)
import           Data.Map                (size, Map, toList)
import qualified Data.SBV                as S
import qualified Data.SBV.Control        as SC
import qualified Data.SBV.Internals      as SI
import           Data.Text               (pack, unpack,Text)
import qualified Data.Text.IO            as T (writeFile)
import           System.IO
import           Text.Megaparsec         (parse)
import           System.IO.Unsafe        (unsafePerformIO)

import           Api
import           CaseStudy.Auto.Auto
import           CaseStudy.Auto.Parser   (langParser)
import           CaseStudy.Auto.Run
import           CaseStudy.Auto.CompactEncode
import           Config
import           Opts
import           Run                     (runAD, runBF)
import           Result
import           Utils
import           VProp.Core
import           VProp.SBV               (toPredicate)
import           VProp.Types


run :: Control.DeepSeq.NFData a => String -> (t -> IO a) -> t -> Benchmark
run !desc !f prop = bench desc $! nfIO (f prop)

-- | make a description for the benchmark, we input pass through variables alg,
-- and confDesc that are hand written names for the algorithm being used and the
-- configuration/prop description. We then input the prop and get a bunch of
-- statistics on it and return all these as a slash '/' separated string
mkDescription :: Ord d => String -> String -> VProp d a b -> String
mkDescription alg confDesc prop = desc
  where
    !desc' = ["Chc",show nChc , "numPlain", show nPln , "Compression", show ratio]
    !desc = mconcat $ intersperse "/" $ pure alg ++ pure confDesc ++ desc'
    !nPln = numPlain prop
    !nChc = numChc prop
    ratio :: Double
    !ratio = fromRational $ compressionRatio prop

-- | Make a benchmark, take two description strings, one to describe the
-- algorithm, one to describe the feature model under analysis, then take a
-- configuration prop, the rest is just pass through parameters to run
-- ex: mkBench "v-->v" "V1"   d0Conf (satWithConf (toDimProp d0Conf) solverConf) bProp
-- ex: mkBench "v-->p" "V1*V2*V3" justD012Conf (bfWith solverConf) justbPropV123
mkBench
  :: (NFData a1, Resultable d) =>
     String
     -> String
     -> VProp d String String
     -> (VProp d a2 b -> IO a1)
     -> VProp d a2 b
     -> Benchmark
mkBench alg confDesc conf !f prop = run desc f prop
  where
    [confPool] = unsafePerformIO $ genConfigPool conf --just call out to the
                                                      --solver, this should
                                                      --always be safe
    (Just prop') = selectVariant confPool prop -- some confs will never be
                                               -- total, so we use select
                                               -- variant here
    desc = mkDescription alg confDesc prop'

-- | a version of mkBench that doesn't require the actual configuration. This is
-- used for instances where the proposition under consideration will be solved
-- projected to a plain term many times, such as in the case of running an
-- evolution aware solution. That is, a variational prop will be fully selected
-- to a plain prop which means that the compression ratio statistics will be
-- meaningless because they only make sense with variational terms.
mkBench'
  :: (NFData a1, Resultable d) =>
     String
     -> String
     -> (VProp d a2 b -> IO a1)
     -> VProp d a2 b
     -> Benchmark
mkBench' alg confDesc !f prop = run desc f prop
  where
    desc = mkDescription alg confDesc prop

-- | make pairs for controlling complexity for compression ratio benchmark. We
-- want to benchmark two versions that have different compression ratios, but
-- that still run only n solver calls. This way the solver calls do not swamp
-- the compression ratio signal
mkPairs :: [a] -> [[a]]
mkPairs [] = [[]]
mkPairs [x] = [[x]]
mkPairs (x:ys@(y:xs)) = [x,y] : mkPairs ys
