module VProp.Boolean where

import VProp.Types
import qualified Data.Map as M ((!), lookup, Map)

import VProp.SBV

-------------------------------- Bool Operations -----------------------------
-- | given a reference variable and a boolean, find the variable in an
-- expression and replace it with a literal
boolSubstitute :: Eq a => (a, Bool) -> VProp d a b -> VProp d a b
boolSubstitute (a, b) d@(RefB x)
  | x == a = LitB b
  | otherwise = d
boolSubstitute new (OpBB op l r) = OpBB op
                                   (boolSubstitute new l)
                                   (boolSubstitute new r)
boolSubstitute new (OpB op e) = OpB op (boolSubstitute new e)
boolSubstitute new (ChcB d l r) = ChcB d
                                  (boolSubstitute new l)
                                  (boolSubstitute new r)
boolSubstitute _ x = x

substitute :: Eq a => [(a, Bool)] -> VProp d a a -> VProp d a a
substitute reps p = foldr boolSubstitute p reps

substitute' :: (Ord a, Eq a) => (M.Map a Bool) -> VProp d a a -> VProp d a a
substitute' reps (RefB x)      = LitB $! reps M.! x
substitute' reps (OpB op e)    = OpB op $ substitute' reps e
substitute' reps (OpBB op l r) = OpBB op (substitute' reps l) (substitute' reps r)
substitute' reps (ChcB d l r)  = ChcB d (substitute' reps l) (substitute' reps r)
substitute' _ (OpIB _ _ _)     = error "Not implememented yet!"
substitute' _ x                = x

solveLiterals :: VProp d a b -> Bool
solveLiterals (LitB b) = b
solveLiterals (OpB _ e) = not $ solveLiterals e
solveLiterals (OpBB op l r) = solveLiterals l `op'` solveLiterals r
  where op' = bDispatch op
solveLiterals (ChcB _ _ _) = error "got a choice when solving literals! Incorrect usage, all choices should be resolved"
solveLiterals _ = error "got a reference, this function should only be applied to props with literals"
