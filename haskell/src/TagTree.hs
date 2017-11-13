module TagTree where

import Data.Maybe (isJust)
import Data.Bifunctor

type Tag = String

type Config a = [(a, Bool)]

data V a b = Obj b
         | Chc a (V a b) (V a b)
         deriving (Eq, Ord)

-- | smart constructor for obj
one :: b -> V a b
one = Obj

-- | smart constructor for chc
chc :: a -> V a b -> V a b -> V a b
chc = Chc

-- | Pull the topmost tag out of a Variational term
tag :: V a b -> Maybe a
tag (Chc t _ _) = Just t
tag _           = Nothing

-- | Pull all the tags out of a Variational term
tags :: V a b -> [a]
tags (Chc t y n) = t : tags y ++ tags n
tags _           = []

-- | Given a variational term, return all objects in it
getAllObjs :: (Integral b) => V a b -> [Integer]
getAllObjs (Chc _ y n) = concatMap getAllObjs [y, n]
getAllObjs (Obj a)     = return $ toInteger a

-- | Given a variation term, if it is an object, get the object
getObj :: V a b -> Maybe b
getObj (Obj a) = Just a
getObj _ = Nothing

-- | Given a variational expression, return true if its an object
isObj :: V a b -> Bool
isObj = isJust . getObj

-- | Given a variational expression, return true if its an choice
isChc :: V a b -> Bool
isChc = not . isObj

-- | Wrapper around engine
prune :: (Eq a) => V a b -> V a b
prune = pruneTagtree []

-- | Given a config and variational expression remove redundant choices
pruneTagtree :: (Eq a) => Config a -> V a b -> V a b
pruneTagtree _ (Obj a) = Obj a
pruneTagtree tb (Chc t y n) = case lookup t tb of
                             Nothing -> Chc t
                                        (pruneTagtree ((t,True):tb) y)
                                        (pruneTagtree ((t,False):tb) n)
                             Just True -> pruneTagtree tb y
                             Just False -> pruneTagtree tb n

-- | Given a configuration and a variational expression perform a selection
select :: (Eq a) => Config a -> V a b -> Maybe b
select _ (Obj a) = Just a
select tbs (Chc t y n) =
  case lookup t tbs of
    Nothing   -> Nothing
    Just True -> select tbs y
    Just False -> select tbs n

instance Functor (V a) where
  fmap f (Obj a) = Obj (f a)
  fmap f (Chc t y n) = Chc t (fmap f y) (fmap f n)


instance Applicative (V a) where
  pure = one
  (Obj f) <*> (Obj e) = Obj $ f e
  f@(Obj _) <*> (Chc t l r) = Chc t (f <*> l) (f <*> r)
  (Chc t fl fr) <*> a@(Obj _) = Chc t (fl <*> a) (fr <*> a)
  (Chc t fl fr) <*> (Chc t' el er) = Chc t
                                     (Chc t' (fl <*> el) (fl <*> er))
                                     (Chc t' (fr <*> el) (fr <*> er))

instance Monad (V a) where
  return  = Obj
  Obj a >>= f = f a
  Chc t y n >>= f = Chc t (y >>= f)(n >>= f)

instance (Show a, Show b) => Show (V a b) where
  show (Obj a)      = show a
  show (Chc t y n)   = show t ++ "<" ++ show y ++ ", " ++ show n ++ ">"

instance Bifunctor V where
  bimap _ g (Obj a) = Obj $ g a
  bimap f g (Chc t l r) = Chc (f t) (bimap f g l) (bimap f g r)
