module Opts where

import System.IO.Unsafe (unsafePerformIO)
import Data.SBV (isSatisfiable)
import GHC.Generics (Generic)

import VProp.Types
import VProp.SBV (SAT, toPredicate)
import qualified Data.Sequence as SE
import Data.Foldable (foldr')
import Data.Monoid ((<>))
import qualified Data.Map.Strict as Map
import Prelude hiding (LT,GT,EQ)

-- | Data type to represent the optimization options
-- Used for the JSON parser
data Opts = MoveRight
          | MoveLeft
          | Shrink
          | Prune
          | Atomize
          | CNF
          | None
          deriving (Generic,Show)


-- | Given any arbritrary prop move any choices to the right
moveChcToRight :: (Ord a, Ord b, Ord d) => VProp d a b -> VProp d a b
  -- structural instances
moveChcToRight !(Opn op xs) = Opn op . SE.sort $ fmap moveChcToRight xs
moveChcToRight !(OpBB XOr x@(ChcB _ _ _) r) = OpBB XOr r x
moveChcToRight !(OpBB BiImpl x@(ChcB _ _ _) r) = OpBB Impl r x
moveChcToRight !(OpIB LT x@(ChcI _ _ _) r) = OpIB GT (moveChcToRight' r) (moveChcToRight' x)
moveChcToRight !(OpIB GT x@(ChcI _ _ _) r) = OpIB LT (moveChcToRight' r) (moveChcToRight' x)
moveChcToRight !(OpIB EQ x@(ChcI _ _ _) r) = OpIB EQ (moveChcToRight' r) (moveChcToRight' x)
moveChcToRight !(OpIB NEQ x@(ChcI _ _ _) r) = OpIB NEQ (moveChcToRight' r) (moveChcToRight' x)
moveChcToRight !(OpIB LTE x@(ChcI _ _ _) r) = OpIB GTE (moveChcToRight' r) (moveChcToRight' x)
moveChcToRight !(OpIB GTE x@(ChcI _ _ _) r) = OpIB LTE (moveChcToRight' r) (moveChcToRight' x)
  -- recursive instances
moveChcToRight !(OpIB op l r) = OpIB op (moveChcToRight' l) (moveChcToRight' r)
moveChcToRight !(ChcB d l r)  = ChcB d  (moveChcToRight l) (moveChcToRight r)
moveChcToRight !(OpB op e)    = OpB op  (moveChcToRight e)
moveChcToRight !(OpBB op l r) = OpBB op (moveChcToRight l) (moveChcToRight r)
moveChcToRight nonRecursive = nonRecursive

moveChcToRight' :: (Ord a) => VIExpr a b -> VIExpr a b
  -- structural instances
moveChcToRight' !(OpII Add x@(ChcI _ _ _) r)  = OpII Add (moveChcToRight' r) (moveChcToRight' x)
moveChcToRight' !(OpII Mult x@(ChcI _ _ _) r) = OpII Mult (moveChcToRight' r) (moveChcToRight' x)
  -- recursive instances
moveChcToRight' !(OpII op l r) = OpII op (moveChcToRight' l) (moveChcToRight' r)
moveChcToRight' !(OpI op e)    = OpI op (moveChcToRight' e)
moveChcToRight' !(ChcI d l r)  = ChcI d (moveChcToRight' l) (moveChcToRight' r)
moveChcToRight' nonRecursive  = nonRecursive

-- | Given any arbritrary prop move any choices to the left
moveChcToLeft :: (Ord a, Ord b, Ord d) => VProp d a b -> VProp d a b
  -- structural instances
moveChcToLeft !(Opn op xs) = Opn op . SE.reverse . SE.sort . fmap moveChcToLeft $ xs
moveChcToLeft !(OpBB XOr    l x@(ChcB _ _ _)) = OpBB XOr (moveChcToLeft x) (moveChcToLeft l)
moveChcToLeft !(OpBB BiImpl l x@(ChcB _ _ _)) = OpBB BiImpl (moveChcToLeft x) (moveChcToLeft l)
moveChcToLeft !(OpIB LT l x@(ChcI _ _ _)) = OpIB GT (moveChcToLeft' x) (moveChcToLeft' l)
moveChcToLeft !(OpIB GT l x@(ChcI _ _ _)) = OpIB LT (moveChcToLeft' x) (moveChcToLeft' l)
moveChcToLeft !(OpIB EQ l x@(ChcI _ _ _)) = OpIB EQ (moveChcToLeft' x) (moveChcToRight' l)
moveChcToLeft !(OpIB NEQ l x@(ChcI _ _ _)) = OpIB NEQ (moveChcToLeft' x) (moveChcToRight' l)
moveChcToLeft !(OpIB LTE l x@(ChcI _ _ _)) = OpIB GTE (moveChcToLeft' x) (moveChcToRight' l)
moveChcToLeft !(OpIB GTE l x@(ChcI _ _ _)) = OpIB LTE (moveChcToLeft' x) (moveChcToRight' l)
  -- recursive instances
moveChcToLeft !(OpIB op l r) = OpIB op (moveChcToLeft' l) (moveChcToLeft' r)
moveChcToLeft !(ChcB d l r)  = ChcB d  (moveChcToLeft l) (moveChcToLeft r)
moveChcToLeft !(OpB op e)    = OpB op  (moveChcToLeft e)
moveChcToLeft !(OpBB op l r) = OpBB op (moveChcToLeft l) (moveChcToLeft r)
moveChcToLeft nonRecursive = nonRecursive

moveChcToLeft' :: (Ord a) => VIExpr a b -> VIExpr a b
  -- structural instances
moveChcToLeft' !(OpII Add l x@(ChcI _ _ _))  = OpII Add (moveChcToLeft' x) (moveChcToLeft' l)
moveChcToLeft' !(OpII Mult l x@(ChcI _ _ _)) = OpII Mult (moveChcToLeft' x) (moveChcToLeft' l)
  -- recursive instances
moveChcToLeft' !(OpII op l r) = OpII op (moveChcToLeft' l) (moveChcToLeft' r)
moveChcToLeft' !(OpI op e)    = OpI op (moveChcToLeft' e)
moveChcToLeft' !(ChcI d l r)  = ChcI d (moveChcToLeft' l) (moveChcToLeft' r)
moveChcToLeft' nonRecursive  = nonRecursive

-- | Given a VProp try to eliminate some terms based on simple rules
shrinkProp :: (Show a, Show d, Show b,  Ord a, Ord b, Ord d) =>
  VProp d a b -> VProp d a b
shrinkProp !(OpB Not (OpB Not x)) = shrinkProp x
shrinkProp !(Opn And xs) = Opn And $ SE.filter (not . tautology) xs
shrinkProp !(Opn Or xs) = Opn Or $ SE.filter (not . unsatisfiable) xs
shrinkProp e
  | unsatisfiable e = false
  | tautology e     = true
shrinkProp !(OpBB op l r) = OpBB op (shrinkProp l) (shrinkProp r)
shrinkProp !(OpB op e) = OpB op $ shrinkProp e
shrinkProp !(ChcB d l r) = ChcB d (shrinkProp l) (shrinkProp r)
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
atomize !x@(ChcB d (OpBB op l r) (OpBB op' l' r'))
  | op == op' = OpBB op
                (ChcB d (atomize l) (atomize l'))
                (ChcB d (atomize r) (atomize r'))
  | otherwise = x
atomize !x@(ChcB d (OpB op l) (OpB op' l'))
  | op == op' = OpB op (ChcB d (atomize l) (atomize l'))
  | otherwise = x
atomize !x@(ChcB d (Opn op es) (Opn op' es'))
  | op == op' = Opn op $ SE.zipWith (\a b -> ChcB d (atomize a) (atomize b)) es es'
  | otherwise = x
  -- recursive instances
atomize !(OpIB op l r) = OpIB op (atomize' l) (atomize' r)
atomize !(Opn op os)   = Opn op (atomize <$> os)
atomize !(OpB op e)    = OpB op (atomize e)
atomize !(OpBB op l r) = OpBB op (atomize l) (atomize r)
atomize !(ChcB d l r)  = ChcB d (atomize l) (atomize r)
atomize x = x

atomize' :: VIExpr d b -> VIExpr d b
  -- structural instances
atomize' !x@(ChcI d (OpII op l r) (OpII op' l' r'))
  | op == op' = OpII op
                (ChcI d (atomize' l) (atomize' l'))
                (ChcI d (atomize' r) (atomize' r'))
  | otherwise = x
atomize' !x@(ChcI d (OpI op l) (OpI op' l'))
  | op == op' = OpI op (ChcI d (atomize' l) (atomize' l'))
  | otherwise = x
  -- recursive instances
atomize' (OpI op e)    = OpI op $ atomize' e
atomize' (OpII op l r) = OpII op (atomize' l) (atomize' r)
atomize' (ChcI d l r)  = ChcI d (atomize' l) (atomize' r)
atomize' x = x

isNormalForm :: VProp d a b -> Bool
isNormalForm !(LitB _) = True
isNormalForm !(RefB _) = True
isNormalForm !(ChcB _ (LitB _) (LitB _ )) = True
isNormalForm !(ChcB _ (RefB _) (LitB _ )) = True
isNormalForm !(ChcB _ (LitB _) (RefB _ )) = True
isNormalForm !(ChcB _ (RefB _) (RefB _ )) = True
isNormalForm !(ChcB _
                (OpIB _ l r)
                (OpIB _ l' r')) = isNormalForm' l && isNormalForm' r &&
                                  isNormalForm' l' && isNormalForm' r'
isNormalForm !(OpIB _ l r) = isNormalForm' l && isNormalForm' r
isNormalForm !(Opn _ os)   = foldr (\x acc -> acc && isNormalForm x) True os
isNormalForm _            = False

isNormalForm' :: VIExpr d b -> Bool
isNormalForm' !(LitI _)  = True
isNormalForm' !(Ref _ _) = True
isNormalForm' !(ChcI _ (LitI _) (LitI _))   = True
isNormalForm' !(ChcI _ (Ref _ _) (LitI _))  = True
isNormalForm' !(ChcI _ (LitI _) (Ref _ _))  = True
isNormalForm' !(ChcI _ (Ref _ _) (Ref _ _)) = True
isNormalForm' !(OpI _ e) = isNormalForm' e
isNormalForm' !(OpII _ l r) = isNormalForm' l && isNormalForm' r
isNormalForm' _ = False


-- | Given a config and variational expression remove redundant choices
prune :: Ord d => VProp d a b -> VProp d a b
prune = prune' Map.empty

prune' :: Ord d => Config d -> VProp d a b -> VProp d a b
prune' tb !(ChcB t y n) = case Map.lookup t tb of
                             Nothing -> ChcB t
                                        (prune' (Map.insert t True tb) y)
                                        (prune' (Map.insert t False tb) n)
                             Just True -> prune' tb y
                             Just False -> prune' tb n
prune' tb !(OpB op x)  = OpB op $ prune' tb x
prune' tb !(OpBB a l r) = OpBB a (prune' tb l) (prune' tb r)
prune' tb !(Opn a ps)  = Opn a (prune' tb <$> ps)
prune' tb !(OpIB op l r)  = OpIB op (prune'' tb l) (prune'' tb r)
prune' _ nonRecursive = nonRecursive

prune'' :: Ord a => Config a -> VIExpr a b -> VIExpr a b
prune'' tb !(ChcI t y n) = case Map.lookup t tb of
                             Nothing -> ChcI t
                                        (prune'' (Map.insert t True tb) y)
                                        (prune'' (Map.insert t False tb) n)
                             Just True -> prune'' tb y
                             Just False -> prune'' tb n
prune'' tb !(OpI op e) = OpI op $ prune'' tb e
prune'' tb !(OpII op l r) = OpII op (prune'' tb l) (prune'' tb r)
prune'' _ nonRecursive = nonRecursive


-- -----------------------------        CNF          ------------------------------
toCNF :: VProp d a b -> VProp d a b
toCNF = associate . distributeAndOverOr . moveNot . elimImplXor

-- | eliminate all implications and equivalences
elimImplXor :: VProp d a b -> VProp d a b
-- elimImplXor !(OpBB BiImpl l r) = Opn And [ Opn Or [bnot l', r']
--                                          , Opn Or [bnot r', l']
--                                          ]

elimImplXor !(OpBB BiImpl l r) = Opn And $
                                 Opn Or lthenr SE.<| SE.singleton (Opn Or rthenl)
  where l' = elimImplXor l
        r' = elimImplXor r
  -- this is the l-then-r, r-then-l pattern in lists:
  -- -- Opn And [ Opn Or [bnot l', r']
            --  , Opn Or [bnot r', l']
            --  ]
        lthenr = (bnot l' SE.<| SE.singleton r')
        rthenl = (bnot r' SE.<| SE.singleton l')
elimImplXor !(OpBB Impl l r) = Opn Or $
                               (bnot $ elimImplXor l) SE.<|
                               (SE.singleton $ elimImplXor r)
  -- xor is elminated via equivalence p `xor` q === (p or q) and (not p or not q)
elimImplXor !(OpBB XOr l r)  = Opn And $
                               Opn Or <$>
                               lthenr SE.<|
                               SE.singleton lthenrNot
  where l' = elimImplXor l
        r' = elimImplXor r
        lthenr = l' SE.<| SE.singleton r'
        lthenrNot = bnot <$> lthenr
elimImplXor !(Opn op os)     = Opn op $ fmap elimImplXor os
elimImplXor !(OpB op e)      = OpB op $ elimImplXor e
elimImplXor !(ChcB d l r)    = ChcB d (elimImplXor l) (elimImplXor r)
elimImplXor nonRecursive    = nonRecursive

-- | apply demorgans repeatedly to move nots inward
moveNot :: VProp d a b -> VProp d a b
moveNot !(OpB Not (Opn And os)) = Opn Or  $ fmap (moveNot . OpB Not) os
moveNot !(OpB Not (Opn Or  os)) = Opn And $ fmap (moveNot . OpB Not) os
moveNot !(OpB Not (OpB Not e))  = e
moveNot !(Opn And os)  = Opn And $ fmap moveNot os
moveNot !(Opn Or os)   = Opn Or $ fmap moveNot os
moveNot !(OpBB op l r) = OpBB op (moveNot l) (moveNot r)
moveNot !(OpB op e)    = OpB op (moveNot e)
moveNot !(ChcB d l r)  = ChcB d (moveNot l) (moveNot r)
moveNot nonRecursive   = nonRecursive

-- | distribute ands over ors
distributeAndOverOr :: VProp d a b -> VProp d a b
distributeAndOverOr (Opn Or es) = foldr1 helper $ fmap distributeAndOverOr es
  where
    helper (Opn And as) x = Opn And $
                            distributeAndOverOr <$>
                            -- this is just list comp e.g.:
                            -- [Opn Or [a,x] | a <- as ]
                            do a <- as
                               return . Opn Or $ a SE.<| SE.singleton x
    helper x (Opn And as) = Opn And $
                            distributeAndOverOr <$>
                            do a <- as
                               return . Opn Or $ x SE.<| SE.singleton a
    helper (Opn Or as) e = Opn Or $ e SE.<| as -- or is commutative
    helper e (Opn Or as) = Opn Or $ e SE.<| as
    helper p q           = Opn Or $ p SE.<| SE.singleton q
distributeAndOverOr (Opn And es) = Opn And $ fmap distributeAndOverOr es
distributeAndOverOr (OpB op e)   = OpB op $ distributeAndOverOr e
distributeAndOverOr (OpBB op l r) = OpBB op
                                    (distributeAndOverOr l)
                                    (distributeAndOverOr r)
distributeAndOverOr (ChcB d l r) = ChcB d
                                   (distributeAndOverOr l)
                                   (distributeAndOverOr r)
distributeAndOverOr nonRecursive = nonRecursive


-- | flatten all nested lists
associate :: VProp d a b -> VProp d a b
associate (Opn And es) = Opn And $ foldr' f SE.empty (fmap associate es)
  where f (Opn And as) bs = as <> bs
        f e bs            = e SE.<| bs
associate (Opn Or es) = Opn Or $ foldr' f SE.empty (fmap associate es)
  where f (Opn Or as) bs = as <> bs
        f e bs            = e SE.<| bs
associate (OpB op e)    = OpB op $ associate e
associate (OpBB op l r) = OpBB op (associate l) (associate r)
associate (ChcB d l r)  = ChcB d (associate l) (associate r)
associate nonRecursive  = nonRecursive
