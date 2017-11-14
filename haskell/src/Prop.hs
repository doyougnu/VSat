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

instance Foldable Prop where
  foldMap f (Lit a)      = f a
  foldMap f (Neg a)      = foldMap f a
  foldMap f (And l r)    = mconcat [foldMap f l, foldMap f r]
  foldMap f (Or l r)     = mconcat [foldMap f l, foldMap f r]
  foldMap f (Impl l r)   = mconcat [foldMap f l, foldMap f r]
  foldMap f (BiImpl l r) = mconcat [foldMap f l, foldMap f r]

  foldr f z (Lit a)      = f a z
  foldr f z (Neg a)      = foldr f (foldr f z a) a
  foldr f z (And l r)    = foldr f (foldr f z l) r
  foldr f z (Or l r)     = foldr f (foldr f z l) r
  foldr f z (Impl l r)   = foldr f (foldr f z l) r
  foldr f z (BiImpl l r) = foldr f (foldr f z l) r

instance Traversable Prop where
  traverse f (Lit a)      = Lit <$> f a
  traverse f (Neg a)      = Neg <$> traverse f a
  traverse f (And l r)    = And    <$> traverse f l <*> traverse f r
  traverse f (Or l r)     = Or     <$> traverse f l <*> traverse f r
  traverse f (Impl l r)   = Impl   <$> traverse f l <*> traverse f r
  traverse f (BiImpl l r) = BiImpl <$> traverse f l <*> traverse f r


-- | Eliminate an biconditionals
elimBi :: Prop a -> Prop a
elimBi (BiImpl a c) = And (Impl a c) (Impl c a)
elimBi x            = x

-- | Eliminate Implications
elimImp :: Prop a -> Prop a
elimImp (Impl a c) = Or (Neg a) c
elimImp x          = x

-- | Demorgans law
deMorgs :: Prop a -> Prop a
deMorgs (Neg (And l r)) = Or (Neg l) (Neg r)
deMorgs (Neg (Or l r))  = And (Neg l) (Neg r)
deMorgs x               = x

-- | Double Negation law
dubNeg :: Prop a -> Prop a
dubNeg (Neg (Neg a)) = a
dubNeg a             = a

-- | Distributive laws
distrib :: Prop a -> Prop a
distrib (And p (Or q r)) = Or (And p q) (And p r)
distrib (And (Or q r) p) = Or (And p q) (And p r)
distrib (Or p (And q r)) = And (Or p q) (Or p r)
distrib (Or (And q r) p) = And (Or p q) (Or p r)
distrib x                = x

-- | Absorption
absorb :: Prop a -> Prop a
absorb (Or p (And p q)) = p
absorb (And p (Or p q)) = p

-- | Given a propositional formulae convert it into conjunctive normal form
-- one expression at a time
_toCNF :: (Show a) => Prop a -> Prop a
_toCNF x@(Impl _ _)   = elimImp x
_toCNF x@(BiImpl _ _) = elimBi x
_toCNF (And l r)    = distrib $ And (_toCNF l) (_toCNF r)
_toCNF (Neg x)      = dubNeg $ Neg (toCNF x)
_toCNF (Or l r)     = distrib $ Or (toCNF l) (toCNF r)
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

-- | traverse a propositional term and pack a list with new elements at each and
toList :: Prop a -> [[a]]
toList term
  | not $ isGround term = []
  | otherwise = undefined

ex :: Prop Integer
ex = And
  (BiImpl (Lit 4) (Lit 1))
  (Or
   (Impl (Lit 1) (Lit 2))
   (And (Lit 8) (Lit 6)))
