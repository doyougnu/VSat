module V where

import Data.Bifunctor         (Bifunctor, bimap)
import Data.Bitraversable     (Bitraversable, bitraverse)
import Data.Bifoldable        (Bifoldable, bifoldMap, bifoldr')
import Data.Foldable          (Foldable)
import Data.Traversable       (Traversable)
import Control.Monad          (ap)
import Data.Monoid            ((<>))
import Data.List              (sortOn)
import qualified Data.Map.Strict     as Map

-- | a choice data type, without the object language
data V d a = Plain a | VChc d (V d a) (V d a) deriving Show

type VConfig d = Map.Map d Bool

instance Bifunctor V where
  bimap _ g (Plain a) = Plain $ g a
  bimap f g (VChc d l r) = VChc (f d) (bimap f g l) (bimap f g r)

instance Bifoldable V where
  bifoldMap _ g (Plain a) = g a
  bifoldMap f g (VChc d l r) = f d <> bifoldMap f g l <> bifoldMap f g r

instance Bitraversable V where
  bitraverse _ g (Plain a) = Plain <$> g a
  bitraverse f g (VChc d l r) = VChc <$> f d <*> bitraverse f g l <*> bitraverse f g r

instance Functor (V d) where fmap = bimap id

instance Foldable (V d) where foldr = bifoldr' (flip const)

instance Traversable (V d) where traverse = bitraverse pure

instance Applicative (V d) where
  pure = Plain
  (<*>) = ap

instance Monad (V d) where
  return = Plain
  (Plain a) >>= f = f a
  (VChc d l r) >>= f = VChc d (l >>= f) (r >>= f)

-- | Given a tag tree, fmap over the tree with respect to a config
replace :: Ord d => VConfig d -> a -> V d (Maybe a) -> V d (Maybe a)
replace _    v (Plain _) = Plain $ Just v
replace conf v (VChc d l r) =
  case Map.lookup d conf of
    Nothing    -> VChc d (replace conf v l) (replace conf v r)
    Just True  -> VChc d (replace conf v l) r
    Just False -> VChc d l (replace conf v r)

-- | helper function used to create seed value for fold just once
_recompile :: Ord d => VConfig d -> a -> V d (Maybe a)
_recompile conf = go (Map.toList conf)
  where
    go :: [(d, Bool)] -> a -> V d (Maybe a)
    go [] val' = Plain . Just $ val'
    go ((d, b):cs) val'
          | b = VChc d (go cs val') (Plain Nothing)
          | otherwise = VChc d (Plain Nothing) (go cs val')

-- | Given a list of configs with associated values, remake the tag tree by
-- folding over the config list
recompile :: Ord d => [(VConfig d, a)] -> Maybe (V d a)
recompile [] = Nothing
recompile xs = sequence $ go (tail xs') (_recompile conf val)
  where
    xs' = reverse $ sortOn (Map.size . fst) xs
    (conf, val) = head xs'
    go :: Ord d => [(VConfig d, a)] -> V d (Maybe a) -> V d (Maybe a)
    go []          acc = acc
    go ((c, v):cs) acc = go cs $ replace c v acc
