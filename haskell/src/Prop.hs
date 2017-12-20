module Prop where

import Utils (parens)
import Control.Monad (ap)

-- | A general propositional language that has all the usual suspects
data Prop a = Lit a                     -- ^ A Literal term
            | Neg    (Prop a)           -- ^ Negation of a term
            | And    (Prop a) (Prop a)  -- ^ A Logical And
            | Or     (Prop a) (Prop a)  -- ^ A Logical Or
            | Impl   (Prop a) (Prop a)  -- ^ A Logical Implication
            | BiImpl (Prop a) (Prop a)  -- ^ A Logical Biconditional
            deriving Eq

-- | A Propositional Language that only allows grounded terms
data GProp a = GLit a                   -- ^ A grounded prop literal
             | GNLit a                  -- ^ A negated grounded literal
             | GAnd (GProp a) (GProp a) -- ^ A grounded and term
             | GOr  (GProp a) (GProp a) -- ^ a ground or term
             deriving Functor

instance (Show a) => Show (Prop a) where
  show (Lit a)          = show a
  show (Neg a)          = "-" ++ show a
  show (And x y)        = parens $ show x ++ " && " ++ show y
  show (Or x y)         = parens $ show x ++ " || " ++ show y
  show (Impl ant con)   = parens $ show ant ++ " -> " ++ show con
  show (BiImpl ant con) = parens $ show ant ++ " <-> " ++ show con

instance (Show a) => Show (GProp a) where
  show (GLit a)          = show a
  show (GNLit a)          = "-" ++ show a
  show (GAnd x y)        = parens $ show x ++ " && " ++ show y
  show (GOr x y)         = parens $ show x ++ " || " ++ show y

instance Functor Prop where
  fmap f (Lit a)      = Lit $ f a
  fmap f (Neg a)      = Neg $ f <$> a
  fmap f (And l r)    = And    (f <$> l) (f <$> r)
  fmap f (Or l r)     = Or     (f <$> l) (f <$> r)
  fmap f (Impl a c)   = Impl   (f <$> a) (f <$> c)
  fmap f (BiImpl a c) = BiImpl (f <$> a) (f <$> c)

instance Applicative Prop where
  pure = Lit
  (<*>) = ap

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

instance Monad Prop where
  return = Lit
  (Lit x) >>= f = f x
  (Neg x) >>= f = Neg $ x >>= f
  (And l r)    >>= f = And    (l >>= f) (r >>= f)
  (Or l r)     >>= f = Or     (l >>= f) (r >>= f)
  (Impl l r)   >>= f = Impl   (l >>= f) (r >>= f)
  (BiImpl l r) >>= f = BiImpl (l >>= f) (r >>= f)

-- | Eliminate an biconditionals
elimBi :: Prop a -> Prop a
elimBi (BiImpl a c) = And
  (Impl (elimBi a) (elimBi c))
  (Impl (elimBi c) (elimBi a))
elimBi (Impl a c) = Impl (elimBi a) (elimBi c)
elimBi (And a c)  = And  (elimBi a) (elimBi c)
elimBi (Or a c)   = Or   (elimBi a) (elimBi c)
elimBi (Neg a)    = Neg  (elimBi a)
elimBi x          = x

-- | Eliminate Implications
elimImp :: Prop a -> Prop a
elimImp (Impl a c) = Or (Neg (elimImp a)) (elimImp c)
elimImp (BiImpl a c) = And
  (Impl (elimImp a) (elimImp c))
  (Impl (elimImp c) (elimImp a))
elimImp (And a c)  = And  (elimImp a) (elimImp c)
elimImp (Or a c)   = Or   (elimImp a) (elimImp c)
elimImp (Neg a)    = Neg  (elimImp a)
elimImp x          = x

-- | Demorgans law
deMorgs :: Prop a -> Prop a
deMorgs (Neg (And l r)) = Or  (Neg l) (Neg r)
deMorgs (Neg (Or l r))  = And (Neg l) (Neg r)
deMorgs (And l r)       = And    (deMorgs l) (deMorgs r)
deMorgs (Or l r)        = Or     (deMorgs l) (deMorgs r)
deMorgs (Impl p q)      = Impl   (deMorgs p) (deMorgs q)
deMorgs (BiImpl p q)    = BiImpl (deMorgs p) (deMorgs q)
deMorgs (Neg q)         = Neg    (deMorgs q)
deMorgs x               = x

-- | Double Negation law
dubNeg :: Prop a -> Prop a
dubNeg (Neg (Neg a)) = a
dubNeg (And l r)    = And    (dubNeg l) (dubNeg r)
dubNeg (Or l r)     = Or     (dubNeg l) (dubNeg r)
dubNeg (Impl p q)   = Impl   (dubNeg p) (dubNeg q)
dubNeg (BiImpl p q) = BiImpl (dubNeg p) (dubNeg q)
dubNeg x            = x

-- | Distributive laws
distrib :: Prop a -> Prop a
distrib (Or p (And q r)) = And
  (Or (distrib p) (distrib q))
  (Or (distrib p) (distrib r))
distrib (Or (And q r) p) = And
  (Or (distrib p) (distrib q))
  (Or (distrib p) (distrib r))
distrib (Or l r)     = Or     (distrib l) (distrib r)
distrib (And a c)    = And    (distrib a) (distrib c)
distrib (Impl p q)   = Impl   (distrib p) (distrib q)
distrib (BiImpl p q) = BiImpl (distrib p) (distrib q)
distrib (Neg q)      = Neg    (distrib q)
distrib x            = x

-- | Absorption
absorb :: (Eq (Prop a)) => Prop a -> Prop a
absorb x@(Or p (And p1 _))
  | p == p1 = p
  | otherwise = x
absorb x@(And p (Or p1 _))
  | p == p1 = p
  | otherwise = x
absorb x = x

-- | True iff a propositional term contains only literals, ands, ors or negations
isCNF :: Prop a -> Bool
isCNF (And l r) = (isAnd l && isAnd r) || (isCNF l && isCNF r)
isCNF (Or l r)  = isCNF l && isCNF r && not (isAnd l) && not (isAnd r)
isCNF (Neg n)   = isCNF n
isCNF (Lit _)   = True
isCNF _         = False

-- | True if the propositional term is an And or the negation of an And
isAnd :: Prop a -> Bool
isAnd (And _ _)       = True
isAnd (Neg (And _ _)) = True
isAnd _               = False

-- | For any Propositional term, reduce it to CNF via logical equivalences
toCNF :: (Show a) => Prop a -> Prop a
toCNF = head . filter isCNF . iterate funcs
  where funcs = dubNeg . distrib . deMorgs . elimImp . elimBi

-- | Convert a propositional term to a grounded term
ground :: (Show a) => Prop a -> GProp a
ground (Lit x)       = GLit x
ground (Neg (Lit x)) = GNLit x
ground (Or l r)      = GOr  (ground l) (ground r)
ground (And l r)     = GAnd (ground l) (ground r)
ground x             = ground $ toCNF x

-- | traverse a propositional term and pack a list with new elements at each and
toListAndSplit :: (Show a) => GProp a -> [GProp a]
toListAndSplit term = go term []
  where
    go (GAnd l r) acc = go l acc ++ go r acc
    go x acc = x : acc

-- | traverse a grounded term that represents Ands via a list, and replace ORs
-- with an inner list, see DIMACs CNF form
orSplit :: (Num a) => [GProp a] -> [[a]]
orSplit = fmap helper
  where
    helper :: (Num a) => GProp a -> [a]
    helper (GLit x)  = [x]
    helper (GNLit x) = [negate x]
    helper (GOr l r) = helper l ++ helper r
    helper _         = [] --this will only ever be an AND, fix the case later

-- | Take any propositional term, ground it, then massage it until it fits the
-- DIMACS CNF clause form
toDimacsProp :: (Num a, Show a) => Prop a -> [[a]]
toDimacsProp = orSplit . toListAndSplit . ground

-- Test Examples
ex :: Prop String
ex = And
  (BiImpl (Lit "a") (Lit "b"))
  (Or
   (Impl (Lit "b") (Lit "c"))
   (And (Lit "d") (Lit "e")))

ex1 :: Prop Integer
ex1 = And
      (Lit 1)
      (Or (Lit 2) (Lit 3))

ex2 :: Prop String
ex2 = Neg
      (And
       (Neg (Lit "p"))
       (Or
         (Lit "q")
         (Neg
           (And
            (Lit "r")
            (Lit "s")))))

ex3 :: Prop String
ex3 = Or
      (And (Lit "p") (Lit "q"))
      (And (Lit "p") (Neg (Lit "q")))
