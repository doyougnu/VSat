module VProp.SBV ( module Data.SBV
                 , andDecomp
                 , shrinkPropExpr
                 , evalPropExpr
                 , symbolicPropExpr) where

import           Data.SBV            ( (&&&)
                                     , (|||)
                                     , (==>)
                                     , (<=>)
                                     , (.==)
                                     , (.<)
                                     , (.>)
                                     , (.<=)
                                     , (.>=)
                                     , Boolean
                                     , Mergeable
                                     , Predicate
                                     , Uninterpreted
                                     , EqSymbolic
                                     , OrdSymbolic
                                     , SBool
                                     , true
                                     , false
                                     , bnot
                                     , symbolicMerge
                                     , unliteral
                                     , sBools
                                     , sInteger
                                     , uninterpret
                                     , ite
                                     , fromBool)
import           Prelude    hiding   (lookup)
import           Data.Maybe          (fromMaybe)
import           Data.Map            (fromList, lookup)
import qualified Data.Set as Set     (toList)

import VProp.Types
import VProp.Core
import SAT

instance Boolean (VProp a) where
  true    = Lit $ B True
  false   = Lit $ B False
  bnot    = Not
  l &&& r = Opn And [l, r]
  l ||| r = Opn Or [l, r]
  (==>)   = Op2 Impl
  (<=>)   = Op2 BiImpl

instance (Show a, Ord a) => SAT (VProp a) where
  toPredicate = symbolicPropExpr

-- | make prop mergeable so choices can use symbolic conditionals
instance Mergeable (VProp a) where
  symbolicMerge _ b thn els
    | Just result <- unliteral b = if result then thn else els
  symbolicMerge _ _ _ _ = undefined -- quite -WALL

instance Eq a => EqSymbolic (VProp a) where
  (.==) l r | l == r = true
            | otherwise = false

instance Ord a => OrdSymbolic (VProp a) where
  (.<) l r | l < r = true
           | otherwise = false

-- TODO fix this repetition
-- | Evaluate a feature expression against a configuration.
evalPropExpr :: DimBool -> VConfig a -> VProp a -> SBool
evalPropExpr _ _  (Lit (B b))   = if b then true else false
evalPropExpr _ _  (Lit (I n))   = uninterpret $ show n
evalPropExpr _ c  (Ref f)   = c f
evalPropExpr d c  (Not e)   = bnot (evalPropExpr d c  e)
evalPropExpr d c  (Opn And ps) = foldr1 (&&&) $ evalPropExpr d c  <$> ps
evalPropExpr d c  (Opn Or ps) = foldr1 (|||) $ evalPropExpr d c <$> ps
evalPropExpr d c  (Op2 Impl l r) = evalPropExpr d c  l ==> evalPropExpr d c r
evalPropExpr d c  (Op2 BiImpl l r) = evalPropExpr d c  l <=> evalPropExpr d c r
evalPropExpr d c  (Op2 VLT l r) = evalPropExpr d c l .< evalPropExpr d c r
evalPropExpr d c  (Op2 VLTE l r) = evalPropExpr d c l .<= evalPropExpr d c r
evalPropExpr d c  (Op2 VGT l r) = evalPropExpr d c l .> evalPropExpr d c r
evalPropExpr d c  (Op2 VGTE l r) = evalPropExpr d c l .>= evalPropExpr d c r
evalPropExpr d c  (Op2 VEQ l r) = evalPropExpr d c l .== evalPropExpr d c r
evalPropExpr d c  (Chc dim l r)
  = ite (d dim) (evalPropExpr d c l) (evalPropExpr d c r)


-- | Generate a symbolic predicate for a feature expression.
symbolicPropExpr :: (Show a, Ord a) => VProp a -> Predicate
symbolicPropExpr e = do
    let vs = Set.toList (vars e)
        ds = Set.toList (dimensions e)
    syms <- fmap (fromList . zip vs) (sBools (show <$> vs))
    dims <- fmap (fromList . zip ds) (sBools (map dimName ds))
    let look f = fromMaybe err (lookup f syms)
        lookd d = fromMaybe errd (lookup d dims)
    return (evalPropExpr lookd look e)
  where err = error "symbolicPropExpr: Internal error, no symbol found."
        errd = error "symbolicPropExpr: Internal error, no dimension found."

-- | Perform andDecomposition, removing all choices from a proposition
andDecomp :: Show a => (VProp a) -> (Dim -> a) -> (VProp a)
andDecomp !(Chc d l r) f = (dimToVar f d &&& andDecomp l f) |||
                          (bnot (dimToVar f d) &&& andDecomp r f)
andDecomp !(Not x)     f = Not (andDecomp x f)
andDecomp !(Op2 c l r) f = Op2 c (andDecomp l f) (andDecomp r f)
andDecomp !(Opn c ps)  f = Opn c (flip andDecomp f <$> ps)
andDecomp !x           _ = x

-- | Reduce the size of a feature expression by applying some basic
--   simplification rules.
shrinkPropExpr :: (Show a, Ord a) => VProp a -> VProp a
shrinkPropExpr e
    | unsatisfiable e           = Lit $ B False
    | tautology e               = Lit $ B True
shrinkPropExpr (Not (Not e))    = shrinkPropExpr e
shrinkPropExpr (Opn And ps)  = Opn And (filter (not . tautology) ps)
shrinkPropExpr (Opn Or ps)   = Opn Or (filter (not . unsatisfiable) ps)
shrinkPropExpr e = e
