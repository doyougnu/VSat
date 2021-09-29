module Core where

import           Control.Arrow           (first, second)
import           Gauge.Main
import           Gauge
import           Control.DeepSeq
import           Control.Monad           (replicateM, foldM, liftM2)
import           Data.List               (sort,splitAt,intersperse,foldl1',delete,(\\),genericLength)
import qualified Data.Set                as Set (size)
import qualified Data.SBV                as S
import qualified Data.SBV.Control        as SC
import qualified Data.SBV.Internals      as SI
import           System.IO
import           Text.Megaparsec         (parse)
import           System.IO.Unsafe        (unsafePerformIO)
import           Data.Function           (on)

import           Api
import           CaseStudy.Auto.Auto
import           CaseStudy.Auto.Run
import           CaseStudy.Auto.CompactEncode
import           Config
import           Opts
import           Run                     (runAD, runBF, vCoreMetrics)
import           Result
import           Utils
import           VProp.Core
import           VProp.Types


run :: Control.DeepSeq.NFData a => String -> (t -> IO a) -> t -> Benchmark
run !desc !f prop = bench desc $! nfIO (f prop)

average :: (Real a, Fractional b) => [a] -> b
average xs = realToFrac (sum xs) / genericLength xs

benchConfig :: Gauge.Config
benchConfig = defaultConfig{minSamples = Just 3, quickMode = True}

singletonsConfig :: Gauge.Config
singletonsConfig = defaultConfig{minSamples = Just 15, quickMode = True}

variateConfig :: Gauge.Config
variateConfig = defaultConfig{quickMode = False, minSamples= Just 56}

-- | make a description for the benchmark, we input pass through variables alg,
-- and confDesc that are hand written names for the algorithm being used and the
-- configuration/prop description. We then input the prop and get a bunch of
-- statistics on it and return all these as a slash '/' separated string
mkDescription :: Resultable d => String -> String -> [ReadableProp d] -> String
mkDescription alg confDesc []   = error "called mkDescription with no props"
mkDescription alg confDesc [prop] = desc
  where
    !desc' = [ "Chc"        , show nChc
             , "numPlain"   , show nPln
             , "Compression", show ratio
             , "VCore_Total", show vCoreTotal
             , "VCorePlain" , show vCorePlain
             , "VCoreVar"   , show vCoreVar
             , "Variants"   , show variants
             ]
    !desc = mconcat $ intersperse "/" $ pure alg ++ pure confDesc ++ desc'
    !nPln = numPlain prop
    !nChc = numChc prop
    ratio :: Double
    !ratio = fromRational $ compressionRatio prop
    !(vCoreTotal, vCorePlain, vCoreVar) = unsafePerformIO $ vCoreMetrics prop
    !variants = 2 ^ (Set.size $ dimensions prop)
-- copying code, the greatest of all possible sins. This just isn't important
-- enough to handle properly
mkDescription alg confDesc props = desc
  where
    !desc' = [ "Chc"        , show nChc
             , "numPlain"   , show nPln
             , "Compression", show ratio
             , "VCore_Total", show vCoreTotal
             , "VCorePlain" , show vCorePlain
             , "VCoreVar"   , show vCoreVar
             , "Variants"   , show variants
             ]
    !desc = mconcat $ intersperse "/" $ pure alg ++ pure confDesc ++ desc'
    !nPln = average $ numPlain <$> props
    !nChc = average $ numChc <$> props
    ratio :: Double
    !ratio = average $ (fromRational . compressionRatio) <$> props
    vCoreTotalSum, vCorePlainSum, vCoreVarSum :: Int
    !(vCoreTotalSum, vCorePlainSum, vCoreVarSum) =
      (foldr (\(x,y,z) (xAcc, yAcc, zAcc) -> (x + xAcc, y + yAcc, z + zAcc)) (0,0,0)
      $ (unsafePerformIO . vCoreMetrics) <$> props)
    !variants = average $ (\p -> 2 ^ (Set.size $ dimensions p)) <$> props
    !l = genericLength props
    myDiv = (/) `on` fromIntegral
    (vCoreTotal, vCorePlain, vCoreVar) = ( vCoreTotalSum `myDiv` l
                                         , vCorePlainSum `myDiv` l
                                         , vCoreVarSum `myDiv` l
                                         )

-- | Make a benchmark, take two description strings, one to describe the
-- algorithm, one to describe the feature model under analysis, then take a
-- configuration prop, the rest is just pass through parameters to run
-- ex: mkBench "v-->v" "V1"   d0Conf (satWithConf (toDimProp d0Conf) solverConf) bProp
-- ex: mkBench "v-->p" "V1*V2*V3" justD012Conf (bfWith solverConf) justbPropV123
mkBench
  :: (NFData a1, Resultable d) =>
     String
     -> String
     -> ReadableProp d
     -> (ReadableProp d -> IO a1)
     -> ReadableProp d
     -> Benchmark
mkBench alg confDesc conf !f prop = run desc f prop
  where
    confPool = unsafePerformIO $ genConfigPool conf --just call out to the
                                                      --solver, this should
                                                      --always be safe
    prop' = flip selectVariant prop <$> confPool  -- some confs will never be
                                                  -- total, so we use select
                                                  -- variant here
    desc = mkDescription alg confDesc prop'

-- | like mkBench but we run compression statistics on the prop directly. It is
-- assumed the prop will have been partially selected to reduce it to the
-- compression dimensions of interest
mkCompBench alg confDesc !f prop = run desc f prop
  where
    desc = mkDescription alg confDesc (pure prop)


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
     -> (ReadableProp d -> IO a1)
     -> ReadableProp d
     -> Benchmark
mkBench' alg confDesc !f prop = run desc f prop
  where
    desc = mkDescription alg confDesc (pure prop)

-- | make pairs for controlling complexity for compression ratio benchmark. We
-- want to benchmark two versions that have different compression ratios, but
-- that still run only n solver calls. This way the solver calls do not swamp
-- the compression ratio signal
mkPairs :: [a] -> [[a]]
mkPairs [] = [[]]
mkPairs [_] = [[]]
mkPairs (x:ys@(y:xs)) = [x,y] : mkPairs ys

-- | Make the compression ratio pair configurations. To Test compression ratio
-- we need to control the number of calls to the solver, so we construct pairs
-- to restrict it to 2 solver calls. Hence if you have 4 features, then we want
-- to test 0-1 1-2 2-3 3-4. The first list should be a list of all dimensions or
-- features, while the second should be a list of pairs
mkCompRatioPairs :: Eq d => [ReadableProp d] -> [[ReadableProp d]] -> [ReadableProp d]
mkCompRatioPairs ds = fmap mkPairConf  . filter (not . (<2) . length)
  where mkPairConf xs@(x:y:_) = (x &&& (negateRest x)) ||| (y &&& (negateRest y))
          where negateRest a = conjoin $ (bnot <$> (ds \\ pure a))

mkCompRatioConfs :: (Resultable d, Eq d) => [ReadableProp d] -> [[ReadableProp d]] -> IO [VProp.Types.Config d]
mkCompRatioConfs ds pairs = mapM (fmap head . genConfigPool . negated) $ filter ((==2) . length) pairs
  where
    negated pair = conjoin $ (bnot <$> (ds \\ pair))
