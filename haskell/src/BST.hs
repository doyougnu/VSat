module BST where

import Data.Bifunctor
import Data.Traversable
import Data.Bitraversable
import Data.Bifoldable
import GHC.Generics


data BST a b = Leaf b
             | Node a (BST a b) (BST a b)
             deriving (Generic)



instance Bifunctor BST where
  bimap _ g (Leaf b) = Leaf $ g b
  bimap f g (Node a l r) = Node (f a) (bimap f g l) (bimap f g r)

instance Functor (BST a) where fmap = bimap id

instance Bifoldable BST where
  bifoldr _ g acc (Leaf b) = g b acc
  bifoldr f g acc (Node n l r) = bifoldr f g (bifoldr f g (f n acc) r) l

instance Foldable (BST a) where foldr = bifoldr $ flip const

instance Bitraversable BST where
  bitraverse _ g (Leaf b) = Leaf <$> g b
  bitraverse f g (Node n l r) = Node <$> f n <*> bitraverse f g l <*> bitraverse f g r

instance Traversable (BST a) where traverse = bitraverse pure

insertWith :: (Eq a,Ord a) => (a -> a -> Ordering) -> a -> b -> BST a b -> BST a b
insertWith _ n b (Leaf a) = Node n (Leaf b) (Leaf a)
insertWith f a b (Node n l r)
  | res == LT = undefined
  | res == EQ = undefined
  | res == GT = undefined
    where res = f a n

-- insert :: (Eq a, Ord a) => a -> b -> BST a b -> BST a b
-- insert n b (Leaf a) =
-- insert a b x@(Node n l r)
--   | a > n = Node n
