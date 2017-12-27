module TagTree where

import Data.Maybe (isJust)
import Data.String
import Data.Bifunctor

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

instance (Monoid b, Data.String.IsString d) => Monoid (V d b) where
  mempty = one mempty
  (Obj d) `mappend` (Obj b) = Obj $ d `mappend` b
  (Chc t l r) `mappend` x@(Obj _)   = Chc t (l `mappend` x) (r `mappend` x)
  x@(Obj _)   `mappend` (Chc t l r) = Chc t (l `mappend` x) (r `mappend` x)
  (Chc t l r) `mappend` (Chc d ll rr) = Chc t
                                        (Chc d (l `mappend` ll) l)
                                        (Chc d (r `mappend` rr) r)

instance Foldable (V d) where
  foldMap f (Obj v) = f v
  foldMap f (Chc _ l r) = foldMap f l `mappend` foldMap f r

instance Traversable (V d) where
  traverse f (Obj v) = Obj <$> f v
  traverse f (Chc d l r) = Chc d <$> traverse f l <*> traverse f r

-- | perform a fold over a choice tree collecting the tags
-- TODO implement profunctor to abstract this out
foldTags :: V d a -> (d -> b -> b) -> b -> b
foldTags (Obj _)     _ acc = acc
foldTags (Chc d l r) f acc = foldTags l f (foldTags r f (f d acc))
