module Api ( sat
           , satWith
           , satWithConf
           , bfWith
           , bf
           , adWith
           , ad
           , DimProp(..)
           , toDimProp
           ) where

import VProp.Types
import VProp.SBV
import SAT
import Config (defConf, debugConf, SMTConf(..), emptyConf)
import Result
import Run
import Utils (fst')

-- | a newtype wrapper to denote that this proposition can only have dimensions
-- as variables
newtype DimProp d a = DimProp {getDimProp :: VProp d (Dim a) (Dim a)}
  deriving (Boolean, SAT)

toDimProp :: VProp d a a -> DimProp d a
toDimProp = DimProp . trimap id Dim Dim

-- | Run VSMT and return variable bindings
sat :: (Show a, Ord a, Ord d, Show d, Resultable d)
  => VProp d a a -> IO (Result d)
sat = satWith defConf

satWith :: (Show a, Ord a, Ord d, Show d, Resultable d)
  => SMTConf d a a -> VProp d a a -> IO (Result d)
satWith = satWithConf Nothing

satWithConf :: (Show a, Ord a, Ord d, Show d, Resultable d)
  => Maybe (DimProp d a) -> SMTConf d a a -> VProp d a a -> IO (Result d)
satWithConf Nothing          conf prop = fst' <$> runVSMT mempty conf prop
satWithConf (Just dimConfig) conf prop =
  do configMap <- getResultMap (trimap id dimName dimName $ getDimProp dimConfig)
     fst' <$> runVSMT (Just configMap) conf prop



bfWith :: (Show a, Ord a, Ord d, Show d, Resultable d)
  => SMTConf d a a -> VProp d a a -> IO (Result d)
bfWith = runBF


bf :: (Show a, Ord a, Ord d, Show d, Resultable d)
  => VProp d a a -> IO (Result d)
bf = bfWith defConf


adWith :: (Show a, Ord a, Ord d, Show d, Resultable d)
  => SMTConf d a a -> (d -> a) -> VProp d a a -> IO (Result d)
adWith conf f prop = runAD conf prop f


ad :: (Show a, Ord a, Ord d, Show d, Resultable d)
  => (d -> a) -> VProp d a a -> IO (Result d)
ad = adWith defConf
