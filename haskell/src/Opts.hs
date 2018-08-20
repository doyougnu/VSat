module Opts where

import System.IO.Unsafe (unsafePerformIO)
import Data.SBV (isSatisfiable)
import GHC.Generics (Generic)

import VProp.Types
import VProp.SBV (SAT, toPredicate)
import Data.List (sort)

-- | Data type to represent the optimization options
-- Used for the JSON parser
data Opts = MoveRight
          | MoveLeft
          | Shrink
          | None
          deriving (Generic,Show)


-- | Given any arbritrary prop move any choices to the right
moveChcToRight :: (Ord a, Ord b) => VProp a b -> VProp a b
moveChcToRight (OpIB op l r) = OpIB op (moveChcToRight' l) (moveChcToRight' r)
moveChcToRight (Opn op xs) = Opn op $ sort xs
moveChcToRight (OpBB XOr x@(ChcB _ _ _) r) = OpBB XOr r x
moveChcToRight (OpBB BiImpl x@(ChcB _ _ _) r) = OpBB Impl r x
moveChcToRight x = x

moveChcToRight' :: (Ord a) => VIExpr a -> VIExpr a
moveChcToRight' (OpII Add x@(ChcI _ _ _) r)  = OpII Add r x
moveChcToRight' (OpII Mult x@(ChcI _ _ _) r) = OpII Mult r x
moveChcToRight' x                            = x

-- | Given any arbritrary prop move any choices to the left
moveChcToLeft :: (Ord a, Ord b) => VProp a b -> VProp a b
moveChcToLeft (OpIB op l r) = OpIB op (moveChcToLeft' l) (moveChcToLeft' r)
moveChcToLeft (Opn op xs) = Opn op . reverse $ sort xs
moveChcToLeft (OpBB XOr l x@(ChcB _ _ _)) = OpBB XOr x l
moveChcToLeft (OpBB BiImpl l x@(ChcB _ _ _)) = OpBB Impl x l
moveChcToLeft x = x

moveChcToLeft' :: (Ord a) => VIExpr a -> VIExpr a
moveChcToLeft' (OpII Add l x@(ChcI _ _ _))  = OpII Add x l
moveChcToLeft' (OpII Mult l x@(ChcI _ _ _)) = OpII Mult x l
moveChcToLeft' x                            = x

-- | Given a VProp try to eliminate some terms based on simple rules
shrinkProp :: (Show a, Ord a) => VProp a a -> VProp a a
shrinkProp (OpB Not (OpB Not x)) = shrinkProp x
shrinkProp (Opn And xs) = Opn And $ filter (not . tautology) xs
shrinkProp (Opn Or xs) = Opn Or $ filter (not . unsatisfiable) xs
shrinkProp e
  | unsatisfiable e = false
  | tautology e     = true
-- | The inner false branch is dead if d == d'
shrinkProp (ChcB d (ChcB d' l' _) r)
  | d == d' = ChcB d (shrinkProp l') (shrinkProp r)
-- | mirrored case
shrinkProp (ChcB d l (ChcB d' _ r'))
  | d == d' = ChcB d (shrinkProp l) (shrinkProp r')
shrinkProp x = x


-- | Is the predicate satisfiable?
satisfiable :: SAT b => b -> Bool
satisfiable b = unsafePerformIO (isSatisfiable (toPredicate b))

-- | Is the predicate unsatisfiable?
unsatisfiable :: SAT b => b -> Bool
unsatisfiable = not . satisfiable

-- | Is the predicate a tautology?
tautology :: SAT b => b -> Bool
tautology = unsatisfiable . bnot

-- | Are these predicates equivalent?
equivalent :: SAT b => b -> b -> Bool
equivalent a b = tautology (a <=> b)
