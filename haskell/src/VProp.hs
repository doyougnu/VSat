module VProp where

import Utils (parens)
import Data.Maybe (isJust)
import Control.Monad (ap)
import Data.List (nub, sortOn)
import Data.Bifunctor
import Data.Bifoldable
import Data.Bitraversable
import qualified Data.Map as M

-- | A general propositional language that has all the usual suspects
data VProp d a = Obj a                           -- ^ A Literal term
               | Neg    (VProp d a)              -- ^ Negation of a term
               | And    (VProp d a) (VProp d a)  -- ^ A Logical And
               | Or     (VProp d a) (VProp d a)  -- ^ A Logical Or
               | Impl   (VProp d a) (VProp d a)  -- ^ A Logical Implication
               | BiImpl (VProp d a) (VProp d a)  -- ^ A Logical Biconditional
               | Chc d  (VProp d a) (VProp d a)  -- ^ A Choice in Dimension d
            deriving Eq

-- | a tag is just a string that represents a dimension in a Choice
type Tag = String

-- | A configuration
type Config d = M.Map d Bool

-- | A Propositional Language that only allows grounded terms
data GProp a = GLit a                   -- ^ A grounded prop literal
             | GNLit a                  -- ^ A negated grounded literal
             | GAnd (GProp a) (GProp a) -- ^ A grounded and term
             | GOr  (GProp a) (GProp a) -- ^ a ground or term
             deriving Functor

instance (Show d, Show a) => Show (VProp d a) where
  show (Obj a)          = show a
  show (Neg a)          = "-" ++ show a
  show (And x y)        = parens $ show x ++ " && " ++ show y
  show (Or x y)         = parens $ show x ++ " || " ++ show y
  show (Impl ant con)   = parens $ show ant ++ " -> " ++ show con
  show (BiImpl ant con) = parens $ show ant ++ " <-> " ++ show con
  show (Chc t y n)   = show t ++ "<" ++ show y ++ ", " ++ show n ++ ">"

instance (Show a) => Show (GProp a) where
  show (GLit a)   = show a
  show (GNLit a)  = "-" ++ show a
  show (GAnd x y) = parens $ show x ++ " && " ++ show y
  show (GOr x y)  = parens $ show x ++ " || " ++ show y

instance Bifunctor VProp where
  bimap _ g (Obj d)      = Obj $ g d
  bimap f g (Chc t l r)  = Chc (f t) (bimap f g l) (bimap f g r)
  bimap f g (Neg a)      = Neg $ bimap f g a
  bimap f g (And l r)    = And (bimap f g l) (bimap f g r)
  bimap f g (Or l r)     = Or (bimap f g l) (bimap f g r)
  bimap f g (Impl a c)   = Impl (bimap f g a) (bimap f g c)
  bimap f g (BiImpl a c) = BiImpl (bimap f g a) (bimap f g c)

instance Functor (VProp d) where
  fmap = bimap id

instance Applicative (VProp d) where
  pure = Obj
  (<*>) = ap

instance Foldable GProp where
  foldMap f (GLit a)   = f a
  foldMap f (GNLit a)  = f a
  foldMap f (GAnd l r) = mconcat [foldMap f l, foldMap f r]
  foldMap f (GOr l r)  = mconcat [foldMap f l, foldMap f r]

instance Bifoldable VProp where
  bifoldr _ g acc (Obj c)      = g c acc
  bifoldr f g acc (Neg a)      = bifoldr f g acc a
  bifoldr f g acc (And l r)    = bifoldr f g (bifoldr f g acc r) l
  bifoldr f g acc (Or l r)     = bifoldr f g (bifoldr f g acc r) l
  bifoldr f g acc (Impl l r)   = bifoldr f g (bifoldr f g acc r) l
  bifoldr f g acc (BiImpl l r) = bifoldr f g (bifoldr f g acc r) l
  bifoldr f g acc (Chc d l r)  = bifoldr f g (bifoldr f g (f d acc) r) l

instance Foldable (VProp d) where
  foldr = bifoldr (flip const)

instance Traversable (VProp d) where
  traverse = bitraverse pure

instance Bitraversable VProp where
  bitraverse _ g (Obj a)      = Obj <$> g a
  bitraverse f g (Neg a)      = Neg <$> bitraverse f g a
  bitraverse f g (And l r)    = And    <$> bitraverse f g l <*> bitraverse f g r
  bitraverse f g (Or l r)     = Or     <$> bitraverse f g l <*> bitraverse f g r
  bitraverse f g (Impl l r)   = Impl   <$> bitraverse f g l <*> bitraverse f g r
  bitraverse f g (BiImpl l r) = BiImpl <$> bitraverse f g l <*> bitraverse f g r
  bitraverse f g (Chc d l r) = Chc <$>
                               f d <*>
                               bitraverse f g l <*> bitraverse f g r

instance Monad (VProp d) where
  return = Obj
  (Obj x) >>= f = f x
  (Neg x) >>= f = Neg $ x >>= f
  (And l r)    >>= f = And    (l >>= f) (r >>= f)
  (Or l r)     >>= f = Or     (l >>= f) (r >>= f)
  (Impl l r)   >>= f = Impl   (l >>= f) (r >>= f)
  (BiImpl l r) >>= f = BiImpl (l >>= f) (r >>= f)
  (Chc t y n)  >>= f = Chc t  (y >>= f)(n >>= f)

------------------- Propositional Formulae Laws --------------------------------
-- all this repetition must be able to be factored out somehow
-- | Eliminate an biconditionals
elimBi :: VProp d a -> VProp d a
elimBi (BiImpl a c) = And
  (Impl (elimBi a) (elimBi c))
  (Impl (elimBi c) (elimBi a))
elimBi (Chc d l r) = Chc d (elimBi l) (elimBi r)
elimBi (Impl a c) = Impl (elimBi a) (elimBi c)
elimBi (And a c)  = And  (elimBi a) (elimBi c)
elimBi (Or a c)   = Or   (elimBi a) (elimBi c)
elimBi (Neg a)    = Neg  (elimBi a)
elimBi x          = x

-- | Eliminate Implications
elimImp :: VProp d a -> VProp d a
elimImp (Impl a c) = Or (Neg (elimImp a)) (elimImp c)
elimImp (BiImpl a c) = And
  (Impl (elimImp a) (elimImp c))
  (Impl (elimImp c) (elimImp a))
elimImp (Chc d l r) = Chc d (elimImp l) (elimImp r)
elimImp (And a c)   = And  (elimImp a) (elimImp c)
elimImp (Or a c)    = Or   (elimImp a) (elimImp c)
elimImp (Neg a)     = Neg  (elimImp a)
elimImp x           = x

-- -- | Demorgans law
deMorgs :: VProp d a -> VProp d a
deMorgs (Neg (And l r)) = Or  (Neg l) (Neg r)
deMorgs (Neg (Or l r))  = And (Neg l) (Neg r)
deMorgs (Chc d l r)     = Chc d  (deMorgs l) (deMorgs r)
deMorgs (And l r)       = And    (deMorgs l) (deMorgs r)
deMorgs (Or l r)        = Or     (deMorgs l) (deMorgs r)
deMorgs (Impl p q)      = Impl   (deMorgs p) (deMorgs q)
deMorgs (BiImpl p q)    = BiImpl (deMorgs p) (deMorgs q)
deMorgs (Neg q)         = Neg    (deMorgs q)
deMorgs x               = x

-- -- | Double Negation law
dubNeg :: VProp d a -> VProp d a
dubNeg (Neg (Neg a)) = a
dubNeg (Chc d l r)  = Chc d  (dubNeg l) (dubNeg r)
dubNeg (And l r)    = And    (dubNeg l) (dubNeg r)
dubNeg (Or l r)     = Or     (dubNeg l) (dubNeg r)
dubNeg (Impl p q)   = Impl   (dubNeg p) (dubNeg q)
dubNeg (BiImpl p q) = BiImpl (dubNeg p) (dubNeg q)
dubNeg x            = x

-- | Distributive laws
distrib :: VProp d a -> VProp d a
distrib (Or p (And q r)) = And
  (Or (distrib p) (distrib q))
  (Or (distrib p) (distrib r))
distrib (Or (And q r) p) = And
  (Or (distrib p) (distrib q))
  (Or (distrib p) (distrib r))
distrib (Chc d l r)  = Chc d  (distrib l) (distrib r)
distrib (Or l r)     = Or     (distrib l) (distrib r)
distrib (And a c)    = And    (distrib a) (distrib c)
distrib (Impl p q)   = Impl   (distrib p) (distrib q)
distrib (BiImpl p q) = BiImpl (distrib p) (distrib q)
distrib (Neg q)      = Neg    (distrib q)
distrib x            = x

-- | Absorption
absorb :: (Eq (VProp d a)) => VProp d a -> VProp d a
absorb x@(Or p (And p1 _))
  | p == p1 = p
  | otherwise = x
absorb x@(And p (Or p1 _))
  | p == p1 = p
  | otherwise = x
absorb x = x

-- | True iff a propositional term contains only literals, ands, ors or negations
isCNF :: VProp d a -> Bool
isCNF (And l r) = (isAnd l && isAnd r) || (isCNF l && isCNF r)
isCNF (Or l r)  = isCNF l && isCNF r && not (isAnd l) && not (isAnd r)
isCNF (Neg n)   = isCNF n
isCNF (Obj _)     = True
isCNF (Chc _ _ _) = True
isCNF _         = False

-- | True if the propositional term is an And or the negation of an And
isAnd :: VProp d a -> Bool
isAnd (And _ _)       = True
isAnd (Neg (And _ _)) = True
isAnd _               = False

-- | For any Propositional term, reduce it to CNF via logical equivalences
toCNF :: VProp d a -> VProp d a
toCNF = head . filter isCNF . iterate funcs
  where funcs = dubNeg . distrib . deMorgs . elimImp . elimBi

---------------------- Choice Functions ----------------------------------------
-- | smart constructor for obj
one :: a -> VProp d a
one = Obj

-- | smart constructor for chc
chc :: d -> VProp d a -> VProp d a -> VProp d a
chc = Chc

-- | Given d variational term, return all objects in it
getAllObjs :: VProp d b -> [b]
getAllObjs (Chc _ y n) = concatMap getAllObjs [y, n]
getAllObjs (Obj d)     = return d
getAllObjs _           = []

-- | Given d variation term, if it is an object, get the object
getObj :: VProp d b -> Maybe b
getObj (Obj d) = Just d
getObj _ = Nothing

-- | Given d variational expression, return true if its an object
isObj :: VProp d b -> Bool
isObj = isJust . getObj

-- | Given d variational expression, return true if its an choice
isChc :: VProp d b -> Bool
isChc = not . isObj

-- | Wrapper around engine
prune :: (Ord d) => VProp d b -> VProp d b
prune = pruneTagTree M.empty

-- | Given d config and variational expression remove redundant choices
pruneTagTree :: (Ord d) => Config d -> VProp d b -> VProp d b
pruneTagTree _ (Obj d) = Obj d
pruneTagTree tb (Chc t y n) = case M.lookup t tb of
                             Nothing -> Chc t
                                        (pruneTagTree (M.insert t True tb) y)
                                        (pruneTagTree (M.insert t False tb) n)
                             Just True -> pruneTagTree tb y
                             Just False -> pruneTagTree tb n
pruneTagTree tb (Neg x)      = Neg $ pruneTagTree tb x
pruneTagTree tb (And l r)    = And    (pruneTagTree tb l) (pruneTagTree tb r)
pruneTagTree tb (Or l r)     = Or     (pruneTagTree tb l) (pruneTagTree tb r)
pruneTagTree tb (Impl l r)   = Impl   (pruneTagTree tb l) (pruneTagTree tb r)
pruneTagTree tb (BiImpl l r) = BiImpl (pruneTagTree tb l) (pruneTagTree tb r)

-- | Given a config and a Variational Prop term select the element out that the
-- config points to
select :: (Ord d) => Config d -> VProp d a -> Maybe (VProp d a)
select _ (Obj a) = Just $ Obj a
select tbs (Chc t y n) = case M.lookup t tbs of
                           Nothing    -> Nothing
                           Just True  -> select tbs y
                           Just False -> select tbs n
select tb (Neg x)      = Neg    <$> select tb x
select tb (And l r)    = And    <$> select tb l <*> select tb r
select tb (Or l r)     = Or     <$> select tb l <*> select tb r
select tb (Impl l r)   = Impl   <$> select tb l <*> select tb r
select tb (BiImpl l r) = BiImpl <$> select tb l <*> select tb r

-- | And Decomposition, convert choices to propositional terms
andDecomp :: VProp a a -> VProp a a
andDecomp (Chc t l r) = Or
                        (And (Obj t)       (andDecomp l))
                        (And (Neg $ Obj t) (andDecomp r))
andDecomp (Obj x)     = Obj x
andDecomp (Neg x)     = Neg (andDecomp x)
andDecomp (Or l r)    = Or (andDecomp l) (andDecomp r)
andDecomp (And l r)   = And (andDecomp l) (andDecomp r)
andDecomp (Impl a c)  = Impl (andDecomp a) (andDecomp c)
andDecomp (BiImpl a c) = BiImpl (andDecomp a) (andDecomp c)

-- | Given a variational term find all paths for the tree in a flat list
paths :: Ord d => VProp d a -> [Config d]
paths = nub . filter (not . M.null) . go
  where
    go (Chc d l r) = do -- TODO: remove nub
      summaryl <- go l
      summaryr <- go r
      [M.insert d True summaryl, M.insert d False summaryr]
    go (Neg x) = go x
    -- TODO cleanup nub and filter calls
    go (And l r)    = go l ++ go r
    go (Or l r)     = go l ++ go r
    go (Impl l r)   = go l ++ go r
    go (BiImpl l r) = go l ++ go r
    go (Obj _) = [M.empty]

-- | Given a tag tree, fmap over the tree with respect to a config
replace :: Ord d => Config d -> (a -> a) -> VProp d (Maybe a) -> VProp d (Maybe a)
replace _    f (Obj a) = Obj $ f <$> a
replace conf f (Chc d l r) = case M.lookup d conf of
  Nothing -> Chc d (replace conf f l) (replace conf f r)
  Just True ->  Chc d (replace conf f l) r
  Just False -> Chc d l (replace conf f r)
replace conf f (Neg x) = Neg $ replace conf f x
replace conf f (And l r)    = And    (replace conf f l) (replace conf f r)
replace conf f (Or l r)     = Or     (replace conf f l) (replace conf f r)
replace conf f (Impl l r)   = Impl   (replace conf f l) (replace conf f r)
replace conf f (BiImpl l r) = BiImpl (replace conf f l) (replace conf f r)

-- | helper function used to create seed value for fold just once
_recompile :: Ord d => Config d -> a -> VProp d (Maybe a)
_recompile conf = go (M.toList conf)
  where
    go :: [(d, Bool)] -> a -> VProp d (Maybe a)
    go [] val' = Obj . Just $ val'
    go ((d, b):cs) val'
          | b = Chc d (go cs val') (Obj Nothing)
          | otherwise = Chc d (Obj Nothing) (go cs val')

-- | Given a list of configs with associated values, remake the tag tree by
-- folding over the config list
recompile :: Ord d => [(Config d, a)] -> Maybe (VProp d a)
recompile [] = Nothing
recompile xs = sequence $ go (tail xs') (_recompile conf val)
  where
    xs' = reverse $ sortOn (M.size . fst) xs
    (conf, val) = head xs'
    go :: Ord d =>
      [(Config d, a)] -> VProp d (Maybe a) -> VProp d (Maybe a)
    go []          acc = acc
    go ((c, v):cs) acc = go cs next
      where next = replace c (const v) acc
---------------------- Language Reduction --------------------------------------
-- | Convert a propositional term to a grounded term
ground :: Ord d => Config d -> VProp d a -> GProp (Maybe a)
ground _ (Obj x)       = GLit . Just $ x
ground _ (Neg (Obj x)) = GNLit . Just $ x
ground c (Or l r)      = GOr  (ground c . toCNF $ l) (ground c . toCNF $ r)
ground c (And l r)     = GAnd (ground c . toCNF $ l) (ground c . toCNF $ r)
ground c x@(Chc _ _ _) = case select c x of
                           Nothing -> GLit Nothing
                           Just a  -> ground c $ toCNF a
ground c x             = ground c $ toCNF x

groundGProp :: Ord d => VProp d a -> GProp a
groundGProp (Obj x) = GLit x
groundGProp (Neg (Obj x)) = GNLit x
groundGProp (Or l r) = GOr (groundGProp . toCNF $ l) (groundGProp . toCNF $ r)
groundGProp (And l r) = GAnd (groundGProp . toCNF $ l) (groundGProp . toCNF $ r)
groundGProp (Chc _ _ _) = error "andDecomp cannot produce a choice, you must have called this without calling andDecomp"
groundGProp x = groundGProp $ toCNF x


-- | traverse a propositional term and pack a list with new elements at each and
toListAndSplit :: GProp a -> [GProp a]
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
-- toDimacsProp :: (Num a, Show a) => VProp d a -> [[a]]
-- toDimacsProp = orSplit . toListAndSplit . ground

-- -- Test Examples
ex :: VProp String String
ex = And
  (BiImpl (Obj "a") (Obj "b"))
  (Or
   (Impl (Obj "b") (Obj "c"))
   (And (Obj "d") (Chc "d" (one "e") (one "e"))))

ex1 :: VProp String Integer
ex1 = And
      (Chc "d"
       (And
         (one 1)
         (Or (one 5) (one 9)))
        (Neg (one 10)))
      (Or (Obj 2) (Obj 3))

ex2 :: VProp String Integer
ex2 = Chc "a" (And (one 1) (one 3)) (Neg $ one 2)

ex3 :: VProp String Integer
ex3 = And (one 1) (Chc "d" (one 3) (one 2))

ex4 :: VProp String Integer
ex4 = Chc "a" (one 1) (one 2)

ex5 :: VProp a Integer
ex5 = And (one 1) (Or (one 2) (one 4))
