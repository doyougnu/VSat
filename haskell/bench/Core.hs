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

-- | keep a desc so that you can add to it later
mkBench
  :: (Ord d, Control.DeepSeq.NFData a1) =>
     String
     -> String
     -> VProp Text String String
     -> (VProp d a2 b -> IO a1)
     -> VProp d a2 b
     -> Benchmark
mkBench alg confDesc conf !f prop = run desc f prop
  where
    !desc' = ["Chc",show nChc , "numPlain", show nPln , "Compression", show ratio]
    !desc = mconcat $ intersperse "/" $ pure alg ++ pure confDesc ++ desc'
    !nPln = numPlain prop
    !nChc = numChc prop
    ratio :: Double
    !ratio = fromRational $ compressionRatio prop
