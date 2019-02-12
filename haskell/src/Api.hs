module Api ( sat
           , satWith
           , bfWith
           , bf
           , adWith
           , ad
           ) where

import VProp.Types
import Config (defConf, debugConf, SMTConf(..), emptyConf)
import Result
import Run
import Utils (fst')

-- | Run VSMT and return variable bindings
sat :: (Show a, Ord a, Ord d, Show d, Resultable d)
  => VProp d a a -> IO (Result d)
sat = satWith defConf

satWith :: (Show a, Ord a, Ord d, Show d, Resultable d)
  => SMTConf d a a -> VProp d a a -> IO (Result d)
satWith = (fmap fst' .) . runVSMT

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
