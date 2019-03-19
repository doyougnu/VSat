module Opts where

import           Data.SBV         (isSatisfiable)
import           GHC.Generics     (Generic)
import           System.IO.Unsafe (unsafePerformIO)

import qualified Data.Map.Strict  as Map
import           Prelude          hiding (EQ, GT, LT)
import           VProp.SBV        (SAT, toPredicate)
import           VProp.Types

import Debug.Trace
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
chcToRight :: (Ord a, Ord b, Ord d) => VProp d a b -> VProp d a b
  -- structural instances
chcToRight (OpBB Impl l r) = chcToRight $ OpBB Or (bnot l) r

  -- base case
chcToRight (OpBB op l@(ChcB _ _ _) r@(ChcB _ _ _)) =
  (OpBB op (chcToRight r) (chcToRight l))

  -- associative move
chcToRight (OpBB op l@(ChcB _ _ _) r) =
  OpBB op (chcToRight r) (chcToRight l)

  -- right tree rotation
chcToRight (OpBB And (OpBB And l' a@(ChcB _ _ _)) r)
  = chcToRight (OpBB And l' (OpBB And r a))

chcToRight (OpBB And (OpBB And a@(ChcB _ _ _) r') r)
  = chcToRight (OpBB And r' (OpBB And r a))

  -- right tree rotation
chcToRight (OpBB Or (OpBB Or l' a@(ChcB _ _ _)) r)
  = chcToRight (OpBB Or l' (OpBB Or r a))

chcToRight (OpBB Or (OpBB Or a@(ChcB _ _ _) r') r)
  = chcToRight (OpBB Or r' (OpBB Or r a))

  -- inner move
chcToRight (OpIB LT x@(ChcI _ _ _) r) = OpIB GT (chcToRight' r) (chcToRight' x)
chcToRight (OpIB GT x@(ChcI _ _ _) r) = OpIB LT (chcToRight' r) (chcToRight' x)
chcToRight (OpIB LTE x@(ChcI _ _ _) r) = OpIB GTE (chcToRight' r) (chcToRight' x)
chcToRight (OpIB GTE x@(ChcI _ _ _) r) = OpIB LTE (chcToRight' r) (chcToRight' x)
chcToRight (OpIB op x@(ChcI _ _ _) r) = OpIB op (chcToRight' r) (chcToRight' x)
  -- recursive instances
chcToRight (OpIB op l r) = OpIB op (chcToRight' l) (chcToRight' r)
chcToRight (ChcB d l r)  = ChcB d  (chcToRight l) (chcToRight r)
chcToRight (OpB op e)    = OpB op  (chcToRight e)
chcToRight (OpBB op l r) = OpBB op (chcToRight l) (chcToRight r)
chcToRight nonRecursive = nonRecursive

chcToRight' :: (Ord a) => VIExpr a b -> VIExpr a b
  -- structural instances
chcToRight' (OpII op x@(ChcI _ _ _) r)  = OpII op (chcToRight' r) (chcToRight' x)
  -- recursive instances
chcToRight' (OpII op l r) = OpII op (chcToRight' l) (chcToRight' r)
chcToRight' (OpI op e)    = OpI op (chcToRight' e)
chcToRight' (ChcI d l r)  = ChcI d (chcToRight' l) (chcToRight' r)
chcToRight' nonRecursive  = nonRecursive

-- | Given any arbritrary prop move any choices to the left
chcToLeft :: (Ord a, Ord b, Ord d, Show d, Show a, Show b) => VProp d a b -> VProp d a b

  -- structural instances
chcToLeft (OpBB Impl l r) = chcToLeft $ OpBB Or (bnot l) r

  -- base case
chcToLeft (OpBB op l@(ChcB _ _ _) r@(ChcB _ _ _)) =
  OpBB op (chcToLeft r) (chcToLeft l)

  -- associative move
chcToLeft (OpBB op l r@(ChcB _ _ _)) =
  OpBB op (chcToLeft r) (chcToLeft l)

  -- left tree rotation and
chcToLeft (OpBB And r (OpBB And l a@(ChcB _ _ _)))
  = chcToLeft (OpBB And a (OpBB And l r))

chcToLeft (OpBB And r (OpBB And a@(ChcB _ _ _) r'))
  = chcToLeft (OpBB And a (OpBB And r r'))

chcToLeft (OpBB And (OpBB And l a@(ChcB _ _ _)) r)
  = chcToLeft (OpBB And a (OpBB And l r))

chcToLeft (OpBB And (OpBB And a@(ChcB _ _ _) r') r)
  = chcToLeft (OpBB And a (OpBB And r r'))

  -- left tree rotation or
chcToLeft (OpBB Or r (OpBB Or l' a@(ChcB _ _ _)))
  = chcToLeft (OpBB Or a (OpBB Or l' r))

chcToLeft (OpBB Or r (OpBB Or a@(ChcB _ _ _) r'))
  = chcToLeft (OpBB Or a (OpBB Or r r'))

  -- structural instances
chcToLeft (OpIB LT l x@(ChcI _ _ _)) = OpIB GT (chcToLeft' x) (chcToLeft' l)
chcToLeft (OpIB GT l x@(ChcI _ _ _)) = OpIB LT (chcToLeft' x) (chcToLeft' l)
chcToLeft (OpIB EQ l x@(ChcI _ _ _)) = OpIB EQ (chcToLeft' x) (chcToRight' l)
chcToLeft (OpIB NEQ l x@(ChcI _ _ _)) = OpIB NEQ (chcToLeft' x) (chcToRight' l)
chcToLeft (OpIB LTE l x@(ChcI _ _ _)) = OpIB GTE (chcToLeft' x) (chcToRight' l)
chcToLeft (OpIB GTE l x@(ChcI _ _ _)) = OpIB LTE (chcToLeft' x) (chcToRight' l)
  -- recursive instances
chcToLeft (OpIB op l r) = OpIB op (chcToLeft' l) (chcToLeft' r)
chcToLeft (OpBB op l r) = OpBB op (chcToLeft l) (chcToLeft r)
chcToLeft (ChcB d l r)  = ChcB d  (chcToLeft l) (chcToLeft r)
chcToLeft (OpB op e)    = OpB op  (chcToLeft e)
chcToLeft nonRecursive = nonRecursive

chcToLeft' :: (Ord a) => VIExpr a b -> VIExpr a b
  -- structural instances
  -- TODO redo these with pattern guards
chcToLeft' (OpII Add l x@(ChcI _ _ _))  = OpII Add (chcToLeft' x) (chcToLeft' l)
chcToLeft' (OpII Mult l x@(ChcI _ _ _)) = OpII Mult (chcToLeft' x) (chcToLeft' l)
chcToLeft' (OpII Sub l x@(ChcI _ _ _)) = OpII Add (negate $ chcToLeft' x) (chcToLeft' l)
  -- recursive instances
chcToLeft' (OpII op l r) = OpII op (chcToLeft' l) (chcToLeft' r)
chcToLeft' (OpI op e)    = OpI op (chcToLeft' e)
chcToLeft' (ChcI d l r)  = ChcI d (chcToLeft' l) (chcToLeft' r)
chcToLeft' nonRecursive  = nonRecursive

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
  | op == op' = atomize $ OpBB op
                (ChcB d l l')
                (ChcB d r r')
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
