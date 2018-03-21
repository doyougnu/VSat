{-# LANGUAGE DeriveAnyClass #-}
module V where

import Data.Bifunctor         (Bifunctor, bimap)
import Data.Bitraversable     (Bitraversable, bitraverse)
import Data.Bifoldable        (Bifoldable, bifoldMap, bifoldr')
import Data.Foldable          (Foldable)
import Data.Traversable       (Traversable)
import Control.Monad          (ap)
import Data.Monoid            ((<>))

-- | a choice data type, without the object language
data V d a = Plain a | VChc d (V d a) (V d a) deriving Show

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
