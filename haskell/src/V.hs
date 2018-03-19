module V where

import Data.Bifunctor         (Bifunctor, bimap)
import Data.Bitraversable     (Bitraversable, bitraverse)
import Data.Bifoldable        (Bifoldable, bifoldMap, bifoldr')
import Data.Foldable          (Foldable)
import Data.Traversable       (Traversable)
import Data.Monoid            ((<>))

-- | a choice data type, without the object language
data V d a = Plain a | Chc d (V d a) (V d a) deriving Show

instance Bifunctor V where
  bimap _ g (Plain a) = Plain $ g a
  bimap f g (Chc d l r) = Chc (f d) (bimap f g l) (bimap f g r)

instance Bifoldable V where
  bifoldMap _ g (Plain a) = g a
  bifoldMap f g (Chc d l r) = f d <> bifoldMap f g l <> bifoldMap f g r

instance Bitraversable V where
  bitraverse _ g (Plain a) = Plain <$> g a
  bitraverse f g (Chc d l r) = Chc <$> f d <*> bitraverse f g l <*> bitraverse f g r

instance Functor (V d) where fmap = bimap id

instance Foldable (V d) where foldr = bifoldr' (flip const)

instance Traversable (V d) where traverse = bitraverse pure
