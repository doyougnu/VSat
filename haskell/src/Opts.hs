module Opts where

import System.IO.Unsafe (unsafePerformIO)
import Data.SBV (isSatisfiable)
import GHC.Generics (Generic)

import VProp.Types
import VProp.Gen
import VProp.SBV (SAT, toPredicate)
import Data.List (sort)
import qualified Data.Map as Map

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

-- | atomization is the process of reshuffling choices in a variational
-- expression such that they are the last node before the leaves in the tree
atomize :: VProp a b -> VProp a b
atomize = undefined

atomize' :: VIExpr a -> VIExpr a
atomize' = undefined

driveChcDown :: VProp a b -> VProp a b
driveChcDown x@(ChcB d
                (OpBB op l r)
                (OpBB op' l' r'))
  | op == op' = OpBB op (ChcB d l l') (ChcB d r r')
  | otherwise = x
driveChcDown x@(ChcB d
               (OpB op l)
               (OpB op' l'))
  | op == op' = OpB op (ChcB d l l')
  | otherwise = x
driveChcDown (OpIB op l r) = OpIB op (driveChcDown' l) (driveChcDown' r)
driveChcDown (Opn op os) = Opn op (driveChcDown <$> os)
driveChcDown (OpB op e) = OpB op (driveChcDown e)
driveChcDown (OpBB op l r) = OpBB op (driveChcDown l) (driveChcDown r)
driveChcDown x = x

driveChcDown' :: VIExpr a -> VIExpr a
driveChcDown' x@(ChcI d
               (OpII op l r)
               (OpII op' l' r'))
  | op == op' = OpII op (ChcI d l l') (ChcI d r r')
  | otherwise = x
driveChcDown' x@(ChcI d (OpI op l) (OpI op' l'))
  | op == op' = OpI op (ChcI d l l')
  | otherwise = x
driveChcDown' x = x

-- | The normal form is CNF with choices driven as close to leaves as possible
isNormalForm :: VProp a b -> Bool
isNormalForm (LitB _) = True
isNormalForm (RefB _) = True
isNormalForm (ChcB _ (LitB _) (LitB _ )) = True
isNormalForm (ChcB _ (RefB _) (LitB _ )) = True
isNormalForm (ChcB _ (LitB _) (RefB _ )) = True
isNormalForm (ChcB _ (RefB _) (RefB _ )) = True
isNormalForm (ChcB _
                (OpIB _ l r)
                (OpIB _ l' r')) = isNormalForm' l && isNormalForm' r &&
                                  isNormalForm' l' && isNormalForm' r'
isNormalForm (OpIB _ l r) = isNormalForm' l && isNormalForm' r
isNormalForm (Opn _ os)   = foldr (\x acc -> acc && isNormalForm x) True os
isNormalForm _            = False

isNormalForm' :: VIExpr a -> Bool
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
prune :: VProp a b -> VProp a b
prune = prune_ Map.empty

prune_ :: Config -> VProp a b -> VProp a b
prune_ tb (ChcB t y n) = case Map.lookup t tb of
                             Nothing -> ChcB t
                                        (prune_ (Map.insert t True tb) y)
                                        (prune_ (Map.insert t False tb) n)
                             Just True -> prune_ tb y
                             Just False -> prune_ tb n
prune_ tb (OpB op x)  = OpB op $ prune_ tb x
prune_ tb (OpBB a l r) = OpBB a (prune_ tb l) (prune_ tb r)
prune_ tb (Opn a ps)  = Opn a (prune_ tb <$> ps)
prune_ tb (OpIB op l r)  = OpIB op (prune_' tb l) (prune_' tb r)
prune_ _ nonRecursive = nonRecursive

prune_' :: Config -> VIExpr a -> VIExpr a
prune_' tb (ChcI t y n) =
  case Map.lookup t tb of
    Nothing -> ChcI t
               (prune_' (Map.insert t True tb) y)
               (prune_' (Map.insert t False tb) n)
    Just True -> prune_' tb y
    Just False -> prune_' tb n
prune_' tb (OpI op e) = OpI op $ prune_' tb e
prune_' tb (OpII op l r) = OpII op (prune_' tb l) (prune_' tb r)
prune_' _ nonRecursive = nonRecursive
