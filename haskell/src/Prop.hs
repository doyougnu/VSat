module Prop where

import Utils (parens)

data Prop a = Lit a                     -- ^ A Literal term
            | Neg (Prop a)              -- ^ Negation of a term
            | And (Prop a) (Prop a)     -- ^ A Logical And
            | Or (Prop a) (Prop a)      -- ^ A Logical Or
            | Impl (Prop a) (Prop a)    -- ^ A Logical Implication
            | BiImpl (Prop a) (Prop a)  -- ^ A Logical Biconditional

instance (Show a) => Show (Prop a) where
  show (Lit a)          = show a
  show (Neg a)          = "-" ++ show a
  show (And x y)        = parens $ show x ++ " && " ++ show y
  show (Or x y)         = parens $ show x ++ " || " ++ show y
  show (Impl ant con)   = parens $ show ant ++ " -> " ++ show con
  show (BiImpl ant con) = parens $ show ant ++ " <-> " ++ show con

instance Functor Prop where
  fmap f (Lit a)      = Lit $ f a
  fmap f (Neg a)      = Neg $ f <$> a
  fmap f (And l r)    = And    (f <$> l) (f <$> r)
  fmap f (Or l r)     = Or     (f <$> l) (f <$> r)
  fmap f (Impl a c)   = Impl   (f <$> a) (f <$> c)
  fmap f (BiImpl a c) = BiImpl (f <$> a) (f <$> c)

-- | Given a propositional formulae convert it into conjunctive normal form
-- one expression at a time
_toCNF :: (Show a) => Prop a -> Prop a
_toCNF (Impl a c)   = Or (Neg a) c
_toCNF (BiImpl a c) = And (Impl a c) (Impl c a)
_toCNF (And l r)    = And (toCNF l) (toCNF r)
_toCNF (Neg x)      = Neg (toCNF x)
_toCNF (Or l r)     = Or (toCNF l) (toCNF r)
_toCNF x            = x

-- | True iff a propositional term contains only literals, ands, ors or negations
isGround :: Prop a -> Bool
isGround (And l r) = isGround l && isGround r
isGround (Or l r)  = isGround l && isGround r
isGround (Neg n)   = isGround n
isGround (Lit _)   = True
isGround _         = False

-- | For any Propositional term, reduce it to CNF and return the CNF form
toCNF :: (Show a) => Prop a -> Prop a
toCNF = head . filter isGround . iterate _toCNF


ex :: Prop Integer
ex = And
  (BiImpl (Lit 4) (Lit 1))
  (Or
   (Impl (Lit 1) (Lit 2))
   (And (Lit 8) (Lit 6)))
