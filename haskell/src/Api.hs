module Api ( SatResult(..)
           , ThmResult(..)
           , sat
           , prove
           , satWith
           , proveWith
           ) where

import Data.SBV (SatResult(..), ThmResult(..))

import VProp.Types
import Config (defConf, debugConf, SMTConf(..))
import Run
import V
import Utils (fst')

-- | Run VSMT and return variable bindings
sat :: VProp String String -> IO [V String (Maybe SatResult)]
sat = satWith defConf

-- | prove a proposition and return a counter example if it exists
prove :: VProp String String -> IO [V String (Maybe ThmResult)]
prove = proveWith defConf

satWith :: SMTConf String -> VProp String String -> IO [V String (Maybe SatResult)]
satWith conf p = fmap (bimap id (fmap SatResult)) . unbox . fst'
                 <$> runVSMT conf p

proveWith :: SMTConf String -> VProp String String -> IO [V String (Maybe ThmResult)]
proveWith conf p = fmap (bimap id (fmap ThmResult)) . unbox . fst'
                   <$> runVSMT conf p
