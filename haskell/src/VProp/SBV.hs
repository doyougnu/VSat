module VProp.SBV ( module Data.SBV
                 , andDecomp
                 -- , shrinkPropExpr
                 , evalPropExpr
                 , symbolicPropExpr) where

import           Data.SBV            ( (&&&)
                                     , (|||)
                                     , (==>)
                                     , (<=>)
                                     , Boolean
                                     , Mergeable
                                     , Predicate
                                     , Uninterpreted
                                     , EqSymbolic
                                     , OrdSymbolic
                                     , SBool
                                     , SInteger
                                     , SDouble
                                     , true
                                     , false
                                     , bnot
                                     , symbolicMerge
                                     , unliteral
                                     , literal
                                     , sBools
                                     , sInteger
                                     , sIntegers
                                     , uninterpret
                                     , ite
                                     , fromBool)
import           Prelude    hiding   (lookup,LT,EQ,GT)
import           Data.Maybe          (fromMaybe)
import           Data.Map            (fromList, lookup)
import qualified Data.Set as Set     (toList)

import VProp.Types
import VProp.Core
import SAT

instance (Show a, Ord a) => SAT (VProp a a) where
  toPredicate = symbolicPropExpr

-- instance Eq a => EqSymbolic (VProp a) where
--   (.==) l r | l == r = true
--             | otherwise = false

-- instance Ord a => OrdSymbolic (VProp a) where
--   (.<) l r | l < r = true
--            | otherwise = false

-- TODO fix this repetition
-- | Evaluate a feature expression against a configuration.
evalPropExpr :: DimBool -> VConfig a SInteger -> VConfig a SBool -> VProp a a -> SBool
evalPropExpr _ _ _ (LitB b)   =  literal b
evalPropExpr _ _ c (RefB f)   = c f
evalPropExpr d i c (OpB Not e)   = bnot (evalPropExpr d i c e)
evalPropExpr d i c (Opn And ps)  = foldr1 (&&&) $ evalPropExpr d i c <$> ps
evalPropExpr d i c (Opn Or ps)   = foldr1 (|||) $ evalPropExpr d i c <$> ps
evalPropExpr d i c (OpBB Impl l r)   = evalPropExpr d i c l ==> evalPropExpr d i c r
evalPropExpr d i c (OpBB BiImpl l r) = evalPropExpr d i c l <=> evalPropExpr d i c r
evalPropExpr d i c (OpBB XOr l r) = evalPropExpr d i c l <+> evalPropExpr d i c r
evalPropExpr d i _ (OpIB op l r) = (handler op) (evalPropExpr' d i l) (evalPropExpr' d i r)
  where handler LT  = (.<)
        handler LTE = (.<=)
        handler GT  = (.>)
        handler GTE = (.>=)
        handler EQ  = (.==)
        handler NEQ = (./=)
evalPropExpr d i c (ChcB dim l r)
  = ite (d dim) (evalPropExpr d i c l) (evalPropExpr d i c r)

-- | Eval the numeric expressions, VIExpr, assume everything is an integer until
-- absolutely necessary to coerce
evalPropExpr' :: DimBool -> VConfig a SInteger -> VIExpr a -> SNum
evalPropExpr' _ _ (LitI (I i)) =  SI . literal $ i
evalPropExpr' _ _ (LitI (D d)) =  SD $ literal d
evalPropExpr' _ i (Ref _ f) = SI $ i f
evalPropExpr' d i (OpI Neg e) = negate $ evalPropExpr' d i e
evalPropExpr' d i (OpI Abs e) = abs $ evalPropExpr' d i e
evalPropExpr' d i (OpI Sign e) = signum $ evalPropExpr' d i e
evalPropExpr' d i (OpII Add l r)  = evalPropExpr' d i l +  evalPropExpr' d i r
evalPropExpr' d i (OpII Sub l r)  = evalPropExpr' d i l -  evalPropExpr' d i r
evalPropExpr' d i (OpII Mult l r) = evalPropExpr' d i l *  evalPropExpr' d i r
evalPropExpr' d i (OpII Div l r)  = evalPropExpr' d i l ./ evalPropExpr' d i r
evalPropExpr' d i (OpII Mod l r)  = evalPropExpr' d i l .% evalPropExpr' d i r
evalPropExpr' d i (ChcI dim l r)
  = ite (d dim) (evalPropExpr' d i l) (evalPropExpr' d i r)

-- | Generate a symbolic predicate for a feature expression.
symbolicPropExpr :: (Show a, Ord a) => VProp a a -> Predicate
symbolicPropExpr e = do
    let vs = Set.toList (vars e)
        is = Set.toList (ivars e)
        ds = Set.toList (dimensions e)
    syms  <- fmap (fromList . zip vs) (sBools (show <$> vs))
    dims  <- fmap (fromList . zip ds) (sBools (map dimName ds))
    isyms <- fmap (fromList . zip is) (sIntegers (show <$> is))
    let look f = fromMaybe err (lookup f syms)
        lookd d = fromMaybe errd (lookup d dims)
        looki i = fromMaybe erri (lookup i isyms)
    return (evalPropExpr lookd looki look e)
  where err = error "symbolicPropExpr: Internal error, no symbol found."
        errd = error "symbolicPropExpr: Internal error, no dimension found."
        erri = error "symbolicPropExpr: Internal error, no int symbol found."

-- | Perform andDecomposition, removing all choices from a proposition
andDecomp :: (Show a) => (VProp a a) -> (Dim -> a) -> (VProp a a)
andDecomp !(ChcB d l r) f = (dimToVar f d &&& andDecomp l f) |||
                            (bnot (dimToVar f d) &&& andDecomp r f)
andDecomp !(OpB op x)    f = OpB  op (andDecomp x f)
andDecomp !(OpBB op l r) f = OpBB op (andDecomp l f) (andDecomp r f)
andDecomp !(Opn op ps)   f = Opn  op (flip andDecomp f <$> ps)
andDecomp !x           _ = x

-- | Reduce the size of a feature expression by applying some basic
--   simplification rules.
-- shrinkPropExpr :: (Show a, Ord a) => VProp a -> VProp a
-- shrinkPropExpr e
--     | unsatisfiable e           = Lit $ B False
--     | tautology e               = Lit $ B True
-- shrinkPropExpr (Not (Not e))    = shrinkPropExpr e
-- shrinkPropExpr (Opn And ps)  = Opn And (filter (not . tautology) ps)
-- shrinkPropExpr (Opn Or ps)   = Opn Or (filter (not . unsatisfiable) ps)
-- shrinkPropExpr e = e
