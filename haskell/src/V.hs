module V where

import           Control.DeepSeq    (NFData)
import           Control.Monad      (ap)
import           Data.Aeson
import           Data.Bifoldable    (Bifoldable, bifoldMap, bifoldr')
import           Data.Bifunctor     (Bifunctor, bimap)
import           Data.Bitraversable (Bitraversable, bitraverse)
import           Data.Foldable      (Foldable)
import           Data.List          (sortOn, partition)
import qualified Data.Map.Strict    as Map
import           Data.Monoid        (Sum (..), (<>))
import qualified Data.Set           as S
import           Data.Traversable   (Traversable)
import           GHC.Generics

import           VProp.Types        (PrimN (..))

-- | a choice data type, without the object language
data V d a = Plain a | VChc d (V d a) (V d a) deriving (Generic,Eq)

type VConfig d = Map.Map d Bool

instance (Show d, Show a) => Show (V d a) where
  show (Plain a)    = show a
  show (VChc d l r) = mconcat [show d, "≺" , show l ,", ", show r, "≻"]

instance (FromJSON a, FromJSON d) => FromJSON (V a d)

instance (NFData a, NFData d) => NFData (V d a)

instance Bifunctor V where
  bimap _ g (Plain a)    = Plain $ g a
  bimap f g (VChc d l r) = VChc (f d) (bimap f g l) (bimap f g r)

instance Bifoldable V where
  bifoldMap _ g (Plain a)    = g a
  bifoldMap f g (VChc d l r) = f d <> bifoldMap f g l <> bifoldMap f g r

instance Bitraversable V where
  bitraverse _ g (Plain a)    = Plain <$> g a
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

-- | In case a custom equality is used
eqWith :: (d -> d -> Bool) -> (a -> a -> Bool) -> V d a -> V d a -> Bool
eqWith f g (VChc d l r) (VChc d' l' r') = f d d'
                                          && eqWith f g l l'
                                          && eqWith f g r r'
eqWith _ g (Plain a) (Plain b) = g a b
eqWith _ _ _         _         = False

-- | Prune all the dead choices in a choice tree
prune :: (Eq a, Eq d) => V d a -> V d a
prune x@(VChc _ l r) | l == r    = r
                     | otherwise = x
prune x                          = x

-- | merge two choice trees with two functions one for dimensions and one for
-- leaves
mergeWith :: (d -> d -> d) -> (a -> a -> b) -> V d a -> V d a -> V d b
mergeWith _ g (Plain a) (Plain b) = Plain $ g a b
mergeWith _ g (Plain a) (VChc d l r) = VChc d (g a <$> l) (g a <$> r)
mergeWith _ g (VChc d l r) (Plain a) = VChc d
                                       ((flip g a) <$> l)
                                       ((flip g a) <$> r)
mergeWith f g (VChc d l r) (VChc d' l' r') = VChc (f d d')
                                             (mergeWith f g l l')
                                             (mergeWith f g r r')


-- | given a list of dimensions construct an empty tag tree
fromList :: [d] -> V d (Maybe a)
fromList []     = Plain Nothing
fromList (x:xs) = VChc x (fromList xs) (fromList xs)

-- | given a list of dimensions, and a default value, construct an tag tree full
-- of default values
fromListWithDefault :: [d] -> a -> V d a
fromListWithDefault []     d = Plain d
fromListWithDefault (x:xs) d = VChc x
                               (fromListWithDefault xs d)
                               (fromListWithDefault xs d)

-- | Given a tag tree, fmap over the tree with respect to a config
replace :: Ord d => (VConfig d, a) -> V d (Maybe a) -> V d (Maybe a)
replace (_,   v) (Plain _) = Plain $ Just v
replace (conf, v) (VChc d l r) =
  case Map.lookup d conf of
    Nothing    -> VChc d (replace (conf, v) l) (replace (conf, v) r)
    Just True  -> VChc d (replace (conf, v) l) r
    Just False -> VChc d l (replace (conf, v) r)

-- | This function replaces whole subtrees based on a config and a value, not
-- just a leaf
replaceNaively :: Ord d => (VConfig d, a) -> V d (Maybe a) -> V d (Maybe a)
replaceNaively (_,   v) (Plain _) = Plain $ Just v
replaceNaively (conf, v) (VChc d l r) =
  case Map.lookup d conf of
    Nothing    -> VChc d (replaceNaively (conf, v) l) (replaceNaively (conf, v) r)
    Just True  -> VChc d (Plain $ Just v) r
    Just False -> VChc d l (Plain $ Just v)

-- | Given a list of configs with associated values, remake the tag tree by
-- folding over the config list
recompile :: (Ord d, Eq a) => [(VConfig d, a)] -> Maybe (V d a)
recompile [] = Nothing
recompile xs = Just $ vSum $ foldr replaceNaively res' ys
  where
    shell = fromList $ Map.keys (fst (head xs'))
    (xs'', ys) = partition (\(x,_) -> Map.size x > 1) $ reverse $ sortOn (Map.size . fst) xs
    xs' = reverse $ sortOn (Map.size . fst) xs
    res' = foldr replace shell xs''

isEmpty :: V d (Maybe a) -> Bool
isEmpty = foldr (\x acc -> check x && acc) True
  where check Nothing = True
        check _       = False

vSum :: (Eq a, Eq d) => V d (Maybe a) -> V d a
vSum (Plain (Just a)) = Plain a
vSum (VChc _ (Plain Nothing) r) = vSum r
vSum (VChc _ l (Plain Nothing)) = vSum l
vSum (VChc d l r)
  -- if the choices doesn't matter then we dissolve it
  |  l == r = vSum r
  -- otherwise we recur
  | otherwise = VChc d (vSum l) (vSum r)
vSum (Plain Nothing) = error "Send the missiles I got to a nothing leaf node"

-- Get the dimensions in a choice tree
numDimensions :: V d a -> Integer
numDimensions = getSum . bifoldMap (const 1) (const mempty)

dimensions :: Ord d => V d a -> S.Set d
dimensions = bifoldr' S.insert (const $ const S.empty) S.empty

-- Predicates
isPlain :: V d a -> Bool
isPlain (Plain _) = True
isPlain _         = False

-- | orient a choice tree based on the ord of its dimensions
dimSort :: Ord d => V d a -> V d a
dimSort (VChc d (VChc d' l' r') (VChc d'' l'' r''))
  | d < d' && d' == d'' = VChc d'
                          (VChc d l' l'')
                          (VChc d r' r'')
  | otherwise = (VChc d
                  (VChc d' (dimSort l') (dimSort r'))
                  (VChc d'' (dimSort l'') (dimSort r'')))
dimSort (VChc d (VChc d' l r) r')
  | d < d' = VChc d'
             (VChc d (dimSort l) (dimSort r'))
             (VChc d (dimSort r) (dimSort r'))
  | otherwise = (VChc d
                 (VChc d' (dimSort l) (dimSort r))
                 (dimSort r'))

dimSort (VChc d l' (VChc d' l r))
  | d < d' = VChc d'
             (VChc d (dimSort l') (dimSort l))
             (VChc d (dimSort l') (dimSort r))
  | otherwise = VChc d
                (dimSort l')
                (VChc d' (dimSort l) (dimSort r))
dimSort x            = x
