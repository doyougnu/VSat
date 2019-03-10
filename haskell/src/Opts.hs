module Opts where

import           Data.SBV         (isSatisfiable)
import           GHC.Generics     (Generic)
import           System.IO.Unsafe (unsafePerformIO)

import qualified Data.Map.Strict  as Map
import           Prelude          hiding (EQ, GT, LT)
import           VProp.SBV        (SAT, toPredicate)
import           VProp.Types

-- | Data type to represent the optimization options
-- Used for the JSON parser
data Opts = MoveRight
          | MoveLeft
          | Shrink
          | Prune
          | Atomize
          | None
          deriving (Generic,Show)


-- | Given any arbritrary prop move any choices to the right
moveChcToRight :: (Ord a, Ord b, Ord d) => VProp d a b -> VProp d a b
  -- structural instances
moveChcToRight (OpBB Impl x@(ChcB _ _ _) r) = OpBB Impl x r
moveChcToRight (OpBB op x@(ChcB _ _ _) r) = OpBB op r x
moveChcToRight (OpIB LT x@(ChcI _ _ _) r) = OpIB GT (moveChcToRight' r) (moveChcToRight' x)
moveChcToRight (OpIB GT x@(ChcI _ _ _) r) = OpIB LT (moveChcToRight' r) (moveChcToRight' x)
moveChcToRight (OpIB LTE x@(ChcI _ _ _) r) = OpIB GTE (moveChcToRight' r) (moveChcToRight' x)
moveChcToRight (OpIB GTE x@(ChcI _ _ _) r) = OpIB LTE (moveChcToRight' r) (moveChcToRight' x)
moveChcToRight (OpIB op x@(ChcI _ _ _) r) = OpIB op (moveChcToRight' r) (moveChcToRight' x)
  -- recursive instances
moveChcToRight (OpIB op l r) = OpIB op (moveChcToRight' l) (moveChcToRight' r)
moveChcToRight (ChcB d l r)  = ChcB d  (moveChcToRight l) (moveChcToRight r)
moveChcToRight (OpB op e)    = OpB op  (moveChcToRight e)
moveChcToRight (OpBB op l r) = OpBB op (moveChcToRight l) (moveChcToRight r)
moveChcToRight nonRecursive = nonRecursive

moveChcToRight' :: (Ord a) => VIExpr a b -> VIExpr a b
  -- structural instances
moveChcToRight' (OpII op x@(ChcI _ _ _) r)  = OpII op (moveChcToRight' r) (moveChcToRight' x)
  -- recursive instances
moveChcToRight' (OpII op l r) = OpII op (moveChcToRight' l) (moveChcToRight' r)
moveChcToRight' (OpI op e)    = OpI op (moveChcToRight' e)
moveChcToRight' (ChcI d l r)  = ChcI d (moveChcToRight' l) (moveChcToRight' r)
moveChcToRight' nonRecursive  = nonRecursive

-- | Given any arbritrary prop move any choices to the left
moveChcToLeft :: (Ord a, Ord b, Ord d) => VProp d a b -> VProp d a b
  -- we match on impl because it is the only non-commutative case
moveChcToLeft (OpBB Impl   l x@(ChcB _ _ _)) = OpBB Impl (moveChcToLeft l) (moveChcToLeft x)
  -- structural instances
moveChcToLeft (OpBB op l x@(ChcB _ _ _)) = OpBB op (moveChcToLeft x) (moveChcToLeft l)
moveChcToLeft (OpIB LT l x@(ChcI _ _ _)) = OpIB GT (moveChcToLeft' x) (moveChcToLeft' l)
moveChcToLeft (OpIB GT l x@(ChcI _ _ _)) = OpIB LT (moveChcToLeft' x) (moveChcToLeft' l)
moveChcToLeft (OpIB EQ l x@(ChcI _ _ _)) = OpIB EQ (moveChcToLeft' x) (moveChcToRight' l)
moveChcToLeft (OpIB NEQ l x@(ChcI _ _ _)) = OpIB NEQ (moveChcToLeft' x) (moveChcToRight' l)
moveChcToLeft (OpIB LTE l x@(ChcI _ _ _)) = OpIB GTE (moveChcToLeft' x) (moveChcToRight' l)
moveChcToLeft (OpIB GTE l x@(ChcI _ _ _)) = OpIB LTE (moveChcToLeft' x) (moveChcToRight' l)
  -- recursive instances
moveChcToLeft (OpIB op l r) = OpIB op (moveChcToLeft' l) (moveChcToLeft' r)
moveChcToLeft (ChcB d l r)  = ChcB d  (moveChcToLeft l) (moveChcToLeft r)
moveChcToLeft (OpB op e)    = OpB op  (moveChcToLeft e)
moveChcToLeft (OpBB op l r) = OpBB op (moveChcToLeft l) (moveChcToLeft r)
moveChcToLeft nonRecursive = nonRecursive

moveChcToLeft' :: (Ord a) => VIExpr a b -> VIExpr a b
  -- structural instances
moveChcToLeft' (OpII Add l x@(ChcI _ _ _))  = OpII Add (moveChcToLeft' x) (moveChcToLeft' l)
moveChcToLeft' (OpII Mult l x@(ChcI _ _ _)) = OpII Mult (moveChcToLeft' x) (moveChcToLeft' l)
  -- recursive instances
moveChcToLeft' (OpII op l r) = OpII op (moveChcToLeft' l) (moveChcToLeft' r)
moveChcToLeft' (OpI op e)    = OpI op (moveChcToLeft' e)
moveChcToLeft' (ChcI d l r)  = ChcI d (moveChcToLeft' l) (moveChcToLeft' r)
moveChcToLeft' nonRecursive  = nonRecursive

-- | Given a VProp try to eliminate some terms based on simple rules
shrinkProp :: (Show a, Show d, Show b,  Ord a, Ord b, Ord d) =>
  VProp d a b -> VProp d a b
shrinkProp (OpB Not (OpB Not x)) = shrinkProp x
shrinkProp e
  | unsatisfiable e = false
  | tautology e     = true
shrinkProp (OpBB op l r) = OpBB op (shrinkProp l) (shrinkProp r)
shrinkProp (OpB op e) = OpB op $ shrinkProp e
shrinkProp (ChcB d l r) = ChcB d (shrinkProp l) (shrinkProp r)
shrinkProp nonRecursive = nonRecursive

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

-- | atomization is the process of reshuffling choices in a variational
-- expression such that they are the last node before the leaves in the tree
atomize :: VProp d a b -> VProp d a b
  -- structural instances
atomize x@(ChcB d (OpBB op l r) (OpBB op' l' r'))
  | op == op' = OpBB op
                (ChcB d (atomize l) (atomize l'))
                (ChcB d (atomize r) (atomize r'))
  | otherwise = x
atomize x@(ChcB d (OpB op l) (OpB op' l'))
  | op == op' = OpB op (ChcB d (atomize l) (atomize l'))
  | otherwise = x
  -- recursive instances
atomize (OpIB op l r) = OpIB op (atomize' l) (atomize' r)
atomize (OpB op e)    = OpB op (atomize e)
atomize (OpBB op l r) = OpBB op (atomize l) (atomize r)
atomize (ChcB d l r)  = ChcB d (atomize l) (atomize r)
atomize x = x

atomize' :: VIExpr d b -> VIExpr d b
  -- structural instances
atomize' x@(ChcI d (OpII op l r) (OpII op' l' r'))
  | op == op' = OpII op
                (ChcI d (atomize' l) (atomize' l'))
                (ChcI d (atomize' r) (atomize' r'))
  | otherwise = x
atomize' x@(ChcI d (OpI op l) (OpI op' l'))
  | op == op' = OpI op (ChcI d (atomize' l) (atomize' l'))
  | otherwise = x
  -- recursive instances
atomize' (OpI op e)    = OpI op $ atomize' e
atomize' (OpII op l r) = OpII op (atomize' l) (atomize' r)
atomize' (ChcI d l r)  = ChcI d (atomize' l) (atomize' r)
atomize' x = x

isNormalForm :: VProp d a b -> Bool
isNormalForm (LitB _) = True
isNormalForm (RefB _) = True
isNormalForm (ChcB _ (LitB _) (LitB _ )) = True
isNormalForm (ChcB _ (RefB _) (LitB _ )) = True
isNormalForm (ChcB _ (LitB _) (RefB _ )) = True
isNormalForm (ChcB _ (RefB _) (RefB _ )) = True
isNormalForm (ChcB _ (RefB _) (OpBB _ _ _)) = True
isNormalForm (ChcB _ (OpBB _ _ _) (RefB _)) = True
isNormalForm (ChcB _ (LitB _) (OpBB _ _ _)) = True
isNormalForm (ChcB _ (OpBB _ _ _) (LitB _)) = True
isNormalForm (ChcB _
               (OpIB _ l r)
               (OpIB _ l' r')) = isNormalForm' l && isNormalForm' r &&
                                 isNormalForm' l' && isNormalForm' r'
isNormalForm (OpIB _ l r) = isNormalForm' l && isNormalForm' r
isNormalForm _            = False

isNormalForm' :: VIExpr d b -> Bool
isNormalForm' (LitI _)  = True
isNormalForm' (Ref _ _) = True
isNormalForm' (ChcI _ (LitI _) (LitI _))   = True
isNormalForm' (ChcI _ (Ref _ _) (LitI _))  = True
isNormalForm' (ChcI _ (LitI _) (Ref _ _))  = True
isNormalForm' (ChcI _ (Ref _ _) (Ref _ _)) = True
isNormalForm' (OpI _ e) = isNormalForm' e
isNormalForm' (OpII _ l r) = isNormalForm' l && isNormalForm' r
isNormalForm' _ = False


-- | Given a config and variational expression remove redundant choices
prune :: Ord d => VProp d a b -> VProp d a b
prune = prune' Map.empty

prune' :: Ord d => Config d -> VProp d a b -> VProp d a b
prune' conf (ChcB d t f) = case Map.lookup d conf of
                             Nothing -> ChcB d
                                        (prune' (Map.insert d True  conf) t)
                                        (prune' (Map.insert d False conf) f)
                             Just True  -> prune' conf t
                             Just False -> prune' conf f
prune' conf (OpB op x)     = OpB op $ prune' conf x
prune' conf (OpBB a l r)   = OpBB a (prune' conf l) (prune' conf r)
prune' conf (OpIB op l r)  = OpIB op (prune'' conf l) (prune'' conf r)
prune' _ nonRecursive = nonRecursive

prune'' :: Ord a => Config a -> VIExpr a b -> VIExpr a b
prune'' conf (ChcI t y n) = case Map.lookup t conf of
                             Nothing -> ChcI t
                                        (prune'' (Map.insert t True conf) y)
                                        (prune'' (Map.insert t False conf) n)
                             Just True -> prune'' conf y
                             Just False -> prune'' conf n
prune'' conf (OpI op e) = OpI op $ prune'' conf e
prune'' conf (OpII op l r) = OpII op (prune'' conf l) (prune'' conf r)
prune'' _ nonRecursive = nonRecursive
