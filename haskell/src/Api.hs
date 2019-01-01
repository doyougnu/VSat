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
           , changeResultType
           , bfWith'
           , bf'
           , bf
           , adWith'
           , ad'
           , ad
           , toSatResult
           , toThmResult
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
sat' :: (Ord a, Show a) => VProp a a -> IO (V String (Maybe S.SMTResult))
sat' = satWith' defConf

-- | map over a function to alter the result type. This is handy to convert
-- SMTResult to SatResult or ThmResult for prettier printing
changeResultType :: (Functor f2, Functor f1, Data.Bifunctor.Bifunctor p) =>
  (a1 -> b1) -> (a2 -> f1 (p b2 (f2 a1))) -> a2 -> f1 (p b2 (f2 b1))
changeResultType f g = fmap (bimap id (fmap f)) . g

sat :: (Ord a, Show a) => VProp a a -> IO (V String (Maybe S.SatResult))
sat = changeResultType S.SatResult sat'

satWith :: (Ord a, Show a) => SMTConf a ->
  VProp a a -> IO (V String (Maybe S.SatResult))
satWith = changeResultType S.SatResult . satWith'

prove :: (Ord a, Show a) => VProp a a -> IO (V String (Maybe S.ThmResult))
prove = changeResultType S.ThmResult prove'

proveWith :: (Ord a, Show a) => SMTConf a -> VProp a a
  -> IO (V String (Maybe S.ThmResult))
proveWith = changeResultType S.ThmResult . proveWith'

-- | prove a proposition and return a counter example if it exists
prove' :: (Show a, Ord a) => VProp a a -> IO (V String (Maybe S.SMTResult))
prove' = proveWith' defConf

satWith' :: (Show a, Ord a) => SMTConf a -> VProp a a
  -> IO (V String (Maybe S.SMTResult))
satWith' = (fmap (unRes . fst') .) . runVSMT

proveWith' :: (Show a, Ord a) => SMTConf a -> VProp a a
  -> IO (V String (Maybe S.SMTResult))
proveWith' conf p = unRes . fst' <$> runVSMT conf p

bfWith' :: (Ord a, Show a) => SMTConf a ->
  VProp a a -> IO (V String (Maybe S.SMTResult))
bfWith' = runBF

bf' :: (Show a, Ord a) => VProp a a -> IO (V String (Maybe S.SMTResult))
bf' = bfWith' defConf

bf :: (Show a, Ord a) => VProp a a -> IO (V String (Maybe S.SatResult))
bf = changeResultType S.SatResult bf'

adWith' :: (Show a, Ord a) => SMTConf a ->
  VProp a a -> IO (V String (Maybe S.SMTResult))
adWith' = runAD

ad' :: (Show a, Ord a) => VProp a a -> IO (V String (Maybe S.SMTResult))
ad' = adWith' defConf

ad :: (Show a, Ord a) => VProp a a -> IO (V String (Maybe S.SatResult))
ad = changeResultType S.SatResult ad'

toSatResult :: (Bifunctor p, Functor f1, Functor f2) =>
  (a2 -> f1 (p b2 (f2 S.SMTResult)))
  -> a2 -> f1 (p b2 (f2 S.SatResult))
toSatResult = changeResultType S.SatResult

toThmResult :: (Bifunctor p, Functor f1, Functor f2) =>
  (a2 -> f1 (p b2 (f2 S.SMTResult)))
  -> a2 -> f1 (p b2 (f2 S.ThmResult))
toThmResult = changeResultType S.ThmResult
