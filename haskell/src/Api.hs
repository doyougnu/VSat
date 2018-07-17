module Api where

import Data.SBV (SatResult(..), ThmResult(..), SMTResult)

import VProp.Types
import Run
import V

sat :: [VProp String String -> VProp String String]
  -> VProp String String
  -> IO [V Dim (Maybe SatResult)]
sat os p = fmap (bimap id (fmap SatResult)) . unbox . fst' <$> runVSMT os p
  where fst' (a,_,_) = a
        unbox :: Result -> [V Dim (Maybe SMTResult)]
        unbox (L _) = []
        unbox (R _) = []
        unbox (V xs) = xs

prove :: [VProp String String -> VProp String String]
  -> VProp String String
  -> IO [V Dim (Maybe ThmResult)]
prove os p = fmap (bimap id (fmap ThmResult)) . unbox . fst' <$> runVSMT os p
  where fst' (a,_,_) = a
        unbox :: Result -> [V Dim (Maybe SMTResult)]
        unbox (L _) = []
        unbox (R _) = []
        unbox (V xs) = xs
