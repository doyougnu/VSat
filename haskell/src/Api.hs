module Api ( S.SatResult(..)
           , S.ThmResult(..)
           , sat'
           , prove'
           , satWith'
           , proveWith'
           , sat
           , prove
           , satWith
           , proveWith
           ) where

import qualified Data.SBV as S
import Data.Bifunctor (Bifunctor)

import VProp.Types
import VProp.Gen (genVProp, vPropShare, genVPropAtShare)
import Config (defConf, debugConf, SMTConf(..), emptyConf)
import Run
import V
import Utils (fst')

-- | Run VSMT and return variable bindings
sat' :: VProp String String -> IO (V String (Maybe S.SMTResult))
sat' = satWith' defConf

-- changeResultType :: (S.SMTResult -> b) ->
--   VProp String String -> IO (V String (Maybe b))

-- | map over a function to alter the result type. This is handy to convert
-- SMTResult to SatResult or ThmResult for prettier printing
changeResultType :: (Functor f2, Functor f1, Data.Bifunctor.Bifunctor p) =>
  (a1 -> b1) -> (a2 -> f1 (p b2 (f2 a1))) -> a2 -> f1 (p b2 (f2 b1))
changeResultType f g = fmap (bimap id (fmap f)) . g

sat :: VProp String String -> IO (V String (Maybe S.SatResult))
sat = changeResultType S.SatResult sat'

satWith :: SMTConf String ->
  VProp String String -> IO (V String (Maybe S.SatResult))
satWith = changeResultType S.SatResult . satWith'

prove :: VProp String String -> IO (V String (Maybe S.ThmResult))
prove = changeResultType S.ThmResult prove'

proveWith :: SMTConf String -> VProp String String
  -> IO (V String (Maybe S.ThmResult))
proveWith = changeResultType S.ThmResult . proveWith'

-- | prove a proposition and return a counter example if it exists
prove' :: VProp String String -> IO (V String (Maybe S.SMTResult))
prove' = proveWith' defConf

satWith' :: SMTConf String -> VProp String String
  -> IO (V String (Maybe S.SMTResult))
satWith' = (fmap (unbox . fst') .) . runVSMT

proveWith' :: SMTConf String -> VProp String String
  -> IO (V String (Maybe S.SMTResult))
proveWith' conf p = unbox . fst' <$> runVSMT conf p
