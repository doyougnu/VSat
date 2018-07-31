module Api ( SatResult(..)
           , ThmResult(..)
           , sat
           , prove
           ) where

import Data.SBV (SatResult(..), ThmResult(..))

import VProp.Types
import Run
import V
import Utils (fst')

sat :: [VProp String String -> VProp String String]
  -> VProp String String
  -> IO [V String (Maybe SatResult)]
sat os p = fmap (bimap id (fmap SatResult)) . unbox . fst' <$> runVSMT os p

prove :: [VProp String String -> VProp String String]
  -> VProp String String
  -> IO [V String (Maybe ThmResult)]
prove os p = fmap (bimap id (fmap ThmResult)) . unbox . fst' <$> runVSMT os p
