module V where

import Data.Bifunctor         (Bifunctor, bimap)
import Data.Bitraversable     (Bitraversable, bitraverse)
import Data.Bifoldable        (Bifoldable, bifoldMap, bifoldr')
import Data.Foldable          (Foldable)
import Data.Traversable       (Traversable)
import qualified Data.Set as S
import Control.Monad          (ap)
import Data.Monoid            ((<>), Sum(..))
import Data.List              (sortOn)
import qualified Data.Map.Strict     as Map
import GHC.Generics
import Control.DeepSeq        (NFData)
import Data.Aeson

import VProp.Types (PrimN(..))

-- | a choice data type, without the object language
data V d a = Plain a | VChc d (V d a) (V d a) deriving (Show,Generic,Eq)

type VConfig d = Map.Map d Bool

instance (FromJSON a, FromJSON d) => FromJSON (V a d)

instance (NFData a, NFData d) => NFData (V d a)

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

instance (Num b,PrimN b) => PrimN (V a b) where
  (Plain a) ./ (Plain b) = Plain $ a ./ b
  (Plain a) ./ x@(VChc _ _ _) = bimap id (\y -> a ./ y) x
  x@(VChc _ _ _) ./ (Plain a) = bimap id (\y -> y ./ a) x
  (VChc d l r) ./ (VChc d' l' r') = VChc d
                                   (VChc d' (l ./ l') (l ./ r'))
                                   (VChc d' (r ./ l') (r ./ r'))
  (Plain a) .% (Plain b) = Plain $ a .% b
  (Plain a) .% x@(VChc _ _ _) = bimap id (\y -> a .% y) x
  x@(VChc _ _ _) .% (Plain a) = bimap id (\y -> y .% a) x
  (VChc d l r) .% (VChc d' l' r') = VChc d
                                   (VChc d' (l .% l') (l .% r'))
                                   (VChc d' (r .% l') (r .% r'))

instance (Num a) => Num (V d a) where
  (Plain a) + (Plain b) = Plain $ a + b
  (Plain a) + x@(VChc _ _ _) = bimap id (+a) x
  x@(VChc _ _ _) + (Plain a) = bimap id (+a) x
  (VChc d l r) + (VChc d' l' r') = VChc d
                                   (VChc d' (l + l') (l + r'))
                                   (VChc d' (r + l') (r + r'))
  (Plain a) * (Plain b) = Plain $ a * b
  (Plain a) * x@(VChc _ _ _) = bimap id (*a) x
  x@(VChc _ _ _) * (Plain a) = bimap id (*a) x
  (VChc d l r) * (VChc d' l' r') = VChc d
                                   (VChc d' (l * l') (l * r'))
                                   (VChc d' (r * l') (r * r'))
  (Plain a) - (Plain b) = Plain $ a - b
  (Plain a) - x@(VChc _ _ _) = bimap id (\y -> a-y) x
  x@(VChc _ _ _) - (Plain a) = bimap id (\y -> y-a) x
  (VChc d l r) - (VChc d' l' r') = VChc d
                                   (VChc d' (l - l') (l - r'))
                                   (VChc d' (r - l') (r - r'))
  abs = bimap id abs
  signum = bimap id signum
  fromInteger = Plain . fromInteger


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


-- Get the dimensions in a choice tree
numDimensions :: V d a -> Integer
numDimensions = getSum . bifoldMap (const 1) (const mempty)

dimensions :: Ord d => V d a -> S.Set d
dimensions = bifoldr' S.insert (const $ const S.empty) S.empty

-- Predicates
isPlain :: V d a -> Bool
isPlain (Plain _) = True
isPlain _          = False
