module VProp.SBV ( andDecomp
                 , evalPropExpr
                 , symbolicPropExpr
                 , bDispatch
                 , nbDispatch
                 , nnDispatch
                 , nDispatch
                 , SAT(..)) where

import qualified Data.SBV as S
import           Prelude    hiding   (lookup,LT,EQ,GT)
import           Data.Maybe          (fromMaybe)
import qualified Data.Map as M       (fromList, lookup)
import qualified Data.Set as Set     (toList)

import VProp.Types
import VProp.Core
import SAT

instance (Show a, Show b, Show d, Ord a, Ord b, Ord d) =>
  SAT (VProp d a b) where toPredicate e = symbolicPropExpr e show show show

-- | convert data constructors to SBV operators, note that the type is
-- purposefully constrained to return SBools and not Boolean b => (b -> b -> b)
-- because we want to ensure that we are translating to the SBV domain
bDispatch :: BB_B -> S.SBool -> S.SBool -> S.SBool
bDispatch And    = (&&&)
bDispatch Or     = (|||)
bDispatch Impl   = (==>)
bDispatch BiImpl = (<=>)
bDispatch XOr    = (<+>)

nbDispatch :: Prim b n => NN_B -> n -> n -> b
nbDispatch LT  = (.<)
nbDispatch LTE = (.<=)
nbDispatch GT  = (.>)
nbDispatch GTE = (.>=)
nbDispatch EQ  = (.==)
nbDispatch NEQ = (./=)

nnDispatch :: PrimN a => NN_N -> a -> a -> a
nnDispatch Add  = (+)
nnDispatch Sub  = (-)
nnDispatch Mult = (*)
nnDispatch Div  = (./)
nnDispatch Mod  = (.%)

nDispatch :: PrimN a => N_N -> a -> a
nDispatch Neg  = negate
nDispatch Sign = signum
nDispatch Abs  = abs

-- | Evaluate a feature expression against a configuration.
evalPropExpr :: DimBool d
             -> VConfig b SNum
             -> VConfig a S.SBool
             -> VProp d a b
             -> S.SBool
evalPropExpr _ _ _ (LitB b)         = S.literal b
evalPropExpr _ _  !c (RefB f)       = c f
evalPropExpr d !i !c !(OpB Not e)   = bnot (evalPropExpr d i c e)
evalPropExpr d !i !c !(OpBB op l r) = (bDispatch op)
                                      (evalPropExpr d i c l)
                                      (evalPropExpr d i c r)
evalPropExpr d !i _  !(OpIB op l r) = (nbDispatch op)
                                      (evalPropExpr' d i l)
                                      (evalPropExpr' d i r)
evalPropExpr d !i !c !(ChcB dim l r)
  = S.ite (d dim) (evalPropExpr d i c l) (evalPropExpr d i c r)

-- | Eval the numeric expressions, VIExpr, assume everything is an integer until
-- absolutely necessary to coerce
evalPropExpr' :: DimBool a -> VConfig b SNum -> VIExpr a b -> SNum
evalPropExpr' _  _ !(LitI (I i)) = SI . S.literal . fromIntegral $ i
evalPropExpr' _  _ !(LitI (D d)) = SD $ S.literal d
evalPropExpr' _ !i !(Ref _ f)    = i f
evalPropExpr' d !i !(OpI op e) = nDispatch op $ evalPropExpr' d i e
evalPropExpr' d !i !(OpII op l r)  = (nnDispatch op)
                                     (evalPropExpr' d i l)
                                     (evalPropExpr' d i r)
evalPropExpr' d !i !(ChcI dim l r)
  = S.ite (d dim) (evalPropExpr' d i l) (evalPropExpr' d i r)

handleNums :: (b -> String) -> (RefN, b) -> S.Symbolic (b, SNum)
handleNums bf (RefI, i) = sequence $ (i, SI <$> S.sInt64 (bf i))
handleNums bf (RefD, d) = sequence $ (d, SD <$> S.sDouble (bf d))

-- | Generate a symbolic predicate for a feature expression.
symbolicPropExpr :: (Ord a, Ord b, Ord d) =>
  VProp d a b -> (d -> String) -> (a -> String) -> (b -> String) -> S.Predicate
symbolicPropExpr e df af bf = do
    let vs = Set.toList (bvars e)
        ds = Set.toList (dimensions e)
        isType = Set.toList (ivarsWithType e)

    syms  <- fmap (M.fromList . zip vs) (S.sBools (af <$> vs))
    dims  <- fmap (M.fromList . zip ds) (S.sBools (map (df . dimName) ds))
    isyms <- M.fromList <$> traverse (handleNums bf) isType
    let look f  = fromMaybe err  (M.lookup f syms)
        lookd d = fromMaybe errd (M.lookup d dims)
        looki i = fromMaybe erri (M.lookup i isyms)
    return (evalPropExpr lookd looki look e)
  where err = error "symbolicPropExpr: Internal error, no symbol found."
        errd = error "symbolicPropExpr: Internal error, no dimension found."
        erri = error "symbolicPropExpr: Internal error, no int symbol found."

-- | Perform andDecomposition, removing all choices from a proposition
andDecomp :: VProp d a b -> (Dim d -> a) -> VProp d a b
andDecomp !(ChcB d l r) f  = (newDim &&& andDecomp l f) |||
                            (bnot newDim &&& andDecomp r f)
  where newDim = dimToVarBy f d
andDecomp !(OpB op x)    f = OpB  op (andDecomp x f)
andDecomp !(OpBB op l r) f = OpBB op (andDecomp l f) (andDecomp r f)
  -- it is unclear how to unwind choices in arithmetic expressions
-- andDecomp !(OpIB op l r) f g = OpIB op (andDecomp' g l) (andDecomp' g r)
andDecomp !x             _ = x
