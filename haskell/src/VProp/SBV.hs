module VProp.SBV ( andDecomp
                 -- , shrinkPropExpr
                 , evalPropExpr
                 , symbolicPropExpr
                 , SAT(..)) where

import qualified Data.SBV as S
import           Prelude    hiding   (lookup,LT,EQ,GT)
import           Data.Maybe          (fromMaybe)
import           Data.Map            (fromList, lookup, size)
import qualified Data.Set as Set     (toList)

import VProp.Types
import VProp.Core
import SAT

import Debug.Trace (trace)


instance (Show a, Ord a) => SAT (VProp a a) where
  toPredicate = symbolicPropExpr

-- TODO fix this repetition
-- | Evaluate a feature expression against a configuration.
evalPropExpr :: DimBool
             -> VConfig a SNum
             -> VConfig a S.SBool
             -> VProp a a
             -> S.SBool
evalPropExpr _ _ _ (LitB b)    = S.literal b
evalPropExpr _ _ !c (RefB f)   = c f
evalPropExpr d !i !c !(OpB Not e)   = S.bnot (evalPropExpr d i c e)
evalPropExpr d !i !c !(Opn And ps)  = foldr1 (&&&) $ evalPropExpr d i c <$> ps
evalPropExpr d !i !c !(Opn Or ps)   = foldr1 (|||) $ evalPropExpr d i c <$> ps
evalPropExpr d !i !c !(OpBB Impl l r)   = evalPropExpr d i c l ==> evalPropExpr d i c r
evalPropExpr d !i !c !(OpBB BiImpl l r) = evalPropExpr d i c l <=> evalPropExpr d i c r
evalPropExpr d !i !c !(OpBB XOr l r) = evalPropExpr d i c l <+> evalPropExpr d i c r
evalPropExpr d !i _  !(OpIB op l r)  = (handler op)
                                       (evalPropExpr' d i l)
                                       (evalPropExpr' d i r)
  where handler LT  = (.<)
        handler LTE = (.<=)
        handler GT  = (.>)
        handler GTE = (.>=)
        handler EQ  = (.==)
        handler NEQ = (./=)
evalPropExpr d !i !c !(ChcB dim l r)
  = S.ite (d dim) (evalPropExpr d i c l) (evalPropExpr d i c r)

-- | Eval the numeric expressions, VIExpr, assume everything is an integer until
-- absolutely necessary to coerce
evalPropExpr' :: DimBool -> VConfig a SNum -> VIExpr a -> SNum
evalPropExpr' _  _ !(LitI (I i)) = SI . S.literal . fromIntegral $ i
evalPropExpr' _  _ !(LitI (D d)) = SD $ S.literal d
evalPropExpr' _ !i !(Ref _ f)    = i f
evalPropExpr' d !i !(OpI Neg e) = negate $ evalPropExpr' d i e
evalPropExpr' d !i !(OpI Abs e) = abs $ evalPropExpr' d i e
evalPropExpr' d !i !(OpI Sign e) = signum $ evalPropExpr' d i e
evalPropExpr' d !i !(OpII Add l r)  = evalPropExpr' d i l +  evalPropExpr' d i r
evalPropExpr' d !i !(OpII Sub l r)  = evalPropExpr' d i l -  evalPropExpr' d i r
evalPropExpr' d !i !(OpII Mult l r) = evalPropExpr' d i l *  evalPropExpr' d i r
evalPropExpr' d !i !(OpII Div l r)  = evalPropExpr' d i l ./ evalPropExpr' d i r
evalPropExpr' d !i !(OpII Mod l r)  = evalPropExpr' d i l .% evalPropExpr' d i r
evalPropExpr' d !i !(ChcI dim l r)
  = S.ite (d dim) (evalPropExpr' d i l) (evalPropExpr' d i r)

-- | Generate a symbolic predicate for a feature expression.
symbolicPropExpr :: (Show a, Ord a) => VProp a a -> S.Predicate
symbolicPropExpr e = do
    let vs = Set.toList (bvars e)
        ds = Set.toList (dimensions e)
        isType = Set.toList (ivarsWithType e)

        helper :: Show a => (RefN, a) -> S.Symbolic (a, SNum)
        helper (RefD, d) = sequence $ (d, SD <$> S.sDouble (show d))
        helper (RefI, i) = sequence $ (i, SI <$> S.sInt64 (show i))

    syms  <- fmap (fromList . zip vs) (S.sBools (show <$> vs))
    dims  <- fmap (fromList . zip ds) (S.sBools (map dimName ds))
    isyms <- fromList <$> traverse helper isType
    let look f  = fromMaybe err  (lookup f syms)
        lookd d = fromMaybe errd (lookup d dims)
        looki i = fromMaybe erri (lookup i isyms)
    return (evalPropExpr lookd looki look e)
  where err = error "symbolicPropExpr: Internal error, no symbol found."
        errd = error "symbolicPropExpr: Internal error, no dimension found."
        erri = error "symbolicPropExpr: Internal error, no int symbol found."

-- | Perform andDecomposition, removing all choices from a proposition
andDecomp :: Show a => (VProp a a) -> (Dim -> a) -> (VProp a a)
andDecomp !(ChcB d l r) f = (dimToVar f d &&& andDecomp l f) |||
                            (S.bnot (dimToVar f d) &&& andDecomp r f)
andDecomp !(OpB op x)    f = OpB  op (andDecomp x f)
andDecomp !(OpBB op l r) f = OpBB op (andDecomp l f) (andDecomp r f)
andDecomp !(Opn op ps)   f = Opn  op (flip andDecomp f <$> ps)
andDecomp !x             _ = x
