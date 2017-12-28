module TagTree where

import Data.Maybe (isJust)
import Data.Bifunctor
import Data.Bifoldable
import Data.Bitraversable
import Data.List (nub)

type Tag = String

type Config d = [(d, Bool)]

data V d b = Obj b
         | Chc d (V d b) (V d b)
         deriving (Eq, Ord)

-- | smart constructor for obj
one :: b -> V d b
one = Obj

-- | smart constructor for chc
chc :: d -> V d b -> V d b -> V d b
chc = Chc

-- | Pull the topmost tag out of d Variational term
tag :: V d b -> Maybe d
tag (Chc t _ _) = Just t
tag _           = Nothing

-- | Pull all the tags out of d Variational term
tags :: V d b -> [d]
tags (Chc t y n) = t : tags y ++ tags n
tags _           = []

-- | Given d variational term, return all objects in it
getAllObjs :: (Integral b) => V d b -> [Integer]
getAllObjs (Chc _ y n) = concatMap getAllObjs [y, n]
getAllObjs (Obj d)     = return $ toInteger d

-- | Given d variation term, if it is an object, get the object
getObj :: V d b -> Maybe b
getObj (Obj d) = Just d
getObj _ = Nothing

-- | Given d variational expression, return true if its an object
isObj :: V d b -> Bool
isObj = isJust . getObj

-- | Given d variational expression, return true if its an choice
isChc :: V d b -> Bool
isChc = not . isObj

-- | Wrapper around engine
prune :: (Eq d) => V d b -> V d b
prune = pruneTagtree []

-- | Given d config and variational expression remove redundant choices
pruneTagtree :: (Eq d) => Config d -> V d b -> V d b
pruneTagtree _ (Obj d) = Obj d
pruneTagtree tb (Chc t y n) = case lookup t tb of
                             Nothing -> Chc t
                                        (pruneTagtree ((t,True):tb) y)
                                        (pruneTagtree ((t,False):tb) n)
                             Just True -> pruneTagtree tb y
                             Just False -> pruneTagtree tb n

-- | Given d configuration and d variational expression perform d selection
select :: (Eq d) => Config d -> V d b -> Maybe b
select _ (Obj d) = Just d
select tbs (Chc t y n) =
  case lookup t tbs of
    Nothing   -> Nothing
    Just True -> select tbs y
    Just False -> select tbs n

instance Functor (V d) where
  fmap f (Obj d) = Obj (f d)
  fmap f (Chc t y n) = Chc t (fmap f y) (fmap f n)

instance Applicative (V d) where
  pure = one
  (Obj f) <*> (Obj e) = Obj $ f e
  f@(Obj _) <*> (Chc t l r) = Chc t (f <*> l) (f <*> r)
  (Chc t fl fr) <*> d@(Obj _) = Chc t (fl <*> d) (fr <*> d)
  (Chc t fl fr) <*> (Chc t' el er) = Chc t
                                     (Chc t' (fl <*> el) (fl <*> er))
                                     (Chc t' (fr <*> el) (fr <*> er))

instance Monad (V d) where
  return  = Obj
  Obj d >>= f = f d
  Chc t y n >>= f = Chc t (y >>= f)(n >>= f)

instance (Show d, Show b) => Show (V d b) where
  show (Obj d)      = show d
  show (Chc t y n)   = show t ++ "<" ++ show y ++ ", " ++ show n ++ ">"

instance Bifunctor V where
  bimap _ g (Obj d) = Obj $ g d
  bimap f g (Chc t l r) = Chc (f t) (bimap f g l) (bimap f g r)

instance Bifoldable V where
  bifoldr _ g acc (Obj c) = g c acc
  bifoldr f g acc (Chc d l r) = bifoldr f g (bifoldr f g (f d acc) r) l

instance Bitraversable V where
  bitraverse _ g (Obj c) = Obj <$> g c
  bitraverse f g (Chc d l r) = Chc <$>
                               f d <*>
                               bitraverse f g l <*> bitraverse f g r

t1 :: V String Integer
t1 = chc "a" (one 1) (one 2)

t2 :: V String Integer
t2 = chc "a"
     (chc "b"
       (one 1)
       (chc "a"
         (one 2)
         (one 3)))
     (one 5)

t3 :: V String Integer
t3 = chc "a"
     (one 1)
     (chc "b"
      (chc "c"
        (one 4)
        (one 5))
       (one 3))

paths :: (Eq d) => V d a -> [Config d]
paths (Obj _) = [[]]
paths (Chc d l r) =
  do
    l' <- [l]
    r' <- [r]
    summaryl <- paths l'
    summaryr <- paths r'
    [nub $ (d, True):summaryl, nub $ (d, False):summaryr] -- TODO fix the nub call
