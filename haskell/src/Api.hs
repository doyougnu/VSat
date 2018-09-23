module Api ( S.SatResult(..)
           , S.ThmResult(..)
           , sat
           , prove
           , satWith
           , proveWith
           ) where

import qualified Data.SBV as S

import VProp.Types
import VProp.Gen (genVProp)
import Config (defConf, debugConf, SMTConf(..))
import Run
import V
import Utils (fst')

-- | Run VSMT and return variable bindings
sat :: VProp String String -> IO (V String (Maybe S.SatResult))
sat = satWith defConf

-- | prove a proposition and return a counter example if it exists
prove :: VProp String String -> IO (V String (Maybe S.ThmResult))
prove = proveWith defConf

satWith :: SMTConf String -> VProp String String
  -> IO (V String (Maybe S.SatResult))
satWith conf p = bimap id (fmap S.SatResult) . unbox . fst'
                 <$> runVSMT conf p

proveWith :: SMTConf String -> VProp String String
  -> IO (V String (Maybe S.ThmResult))
proveWith conf p = bimap id (fmap S.ThmResult) . unbox . fst'
                   <$> runVSMT conf p
