module VProp ( VProp (..)
             , Config
             , Op2 (..)
             , _impl
             , _and
             , _or
             , numChc
             , numTerms
             , depth
             , recompile
             , paths
             , andDecomp
             , select
             , one
             , chc
             , prune) where

import Utils              (parens)
import Control.Monad      (liftM3, liftM2)
import Data.List          (nub, sortOn)
import GHC.Generics       (Generic)
import Control.DeepSeq    (NFData)
import Data.Bifunctor     (Bifunctor, bimap)
import Data.Bifoldable    (Bifoldable, bifoldr', bifoldr)
import Data.Bitraversable (Bitraversable, bitraverse)
import Test.QuickCheck    (Arbitrary, Gen, arbitrary, frequency, sized)
import qualified Data.Map.Strict as M
import Data.Data (Data,Typeable)

import qualified NVProp as P

-- | A general propositional language that has all the usual suspects
data VProp d a = Ref !a                            -- ^ A literal result of a choice
               | Neg !(VProp d a)                  -- ^ Negation
               | Op2 Op2 !(VProp d a) !(VProp d a) -- ^ Binary Terms
               | Chc d  !(VProp d a) !(VProp d a)  -- ^ A Choice in Dimension d
            deriving (Eq, Generic, Data, Typeable)

newtype Feature = Feature { featureName :: String }
  deriving (Data,Eq,IsString,Ord,Show,Typeable)

newtype Feature = Feature { featureName :: String }
  deriving (Data,Eq,IsString,Ord,Show,Typeable)
data Op2 = And | Or | Impl | BiImpl deriving (Eq, Generic, Data, Typeable)

-- | smart constructors
_and :: VProp d a -> VProp d a -> VProp d a
_and = Op2 And

_or:: VProp d a -> VProp d a -> VProp d a
_or = Op2 Or

_impl:: VProp d a -> VProp d a -> VProp d a
_impl = Op2 Impl

_bimpl :: VProp d a -> VProp d a -> VProp d a
_bimpl = Op2 BiImpl

-- | A configuration
type Config d = M.Map d Bool

instance Show Op2 where
  show And    = " && "
  show Or     = " || "
  show Impl   = " -> "
  show BiImpl = " <-> "

instance (Show d, Show a) => Show (VProp d a) where
  show (Ref a)      = show a
  show (Neg a)      = "-" ++ show a
  show (Op2 a l r)  = parens $ show l ++ show a ++ show r
  show (Chc t y n)  = show t ++ "<" ++ show y ++ ", " ++ show n ++ ">"

instance Bifunctor VProp where
  bimap _ g (Ref d)      = Ref $ g d
  bimap f g (Chc t l r)  = Chc (f t) (bimap f g l) (bimap f g r)
  bimap f g (Neg a)      = Neg $ bimap f g a
  bimap f g (Op2 a l r)  = Op2 a (bimap f g l) (bimap f g r)

instance Functor (VProp d) where
  fmap = bimap id

instance Bifoldable VProp where
  bifoldr _ g acc (Ref c)      = g c acc
  bifoldr f g acc (Chc d l r)  = bifoldr f g (bifoldr f g (f d acc) r) l
  bifoldr f g acc (Neg a)      = bifoldr f g acc a
  bifoldr f g acc (Op2 _ l r)  = bifoldr f g (bifoldr f g acc r) l

instance Foldable (VProp d) where
  foldr = bifoldr' (flip const)

instance Traversable (VProp d) where
  traverse = bitraverse pure

instance Bitraversable VProp where
  bitraverse _ g (Ref a)      = Ref <$> g a
  bitraverse f g (Neg a)      = Neg <$> bitraverse f g a
  bitraverse f g (Op2 a l r)  = Op2 a <$> bitraverse f g l <*> bitraverse f g r
  bitraverse f g (Chc d l r)  = Chc <$>
                                f d <*>
                                bitraverse f g l <*> bitraverse f g r

instance (Arbitrary d, Arbitrary a) => Arbitrary (VProp d a) where
  arbitrary = sized arbVProp

instance (NFData d, NFData a) => NFData (VProp d a)
instance NFData Op2

-- | Generate an Arbitrary VProp, these frequencies can change for different
-- depths
arbVProp :: (Arbitrary a, Arbitrary d) => Int -> Gen (VProp d a)
arbVProp 0 = fmap Ref arbitrary
arbVProp n = frequency [ (1, fmap Ref arbitrary)
                       , (4, liftM3 Chc arbitrary l l)
                       , (3, fmap Neg l)
                       , (3, liftM2 _or l l)
                       , (3, liftM2 _and l l)
                       , (3, liftM2 _impl l l)
                       , (3, liftM2 _bimpl l l)
                       ]
  where l = arbVProp (n `div` 2)

------------------- Propositional Formulae Laws --------------------------------
-- | Eliminate an biconditionals
elimBi :: VProp d a -> VProp d a
elimBi !(Op2 BiImpl a c) = _and
                          (_impl (elimBi a) (elimBi c))
                          (_impl (elimBi c) (elimBi a))
elimBi !(Chc d l r) = Chc d (elimBi l) (elimBi r)
elimBi !(Op2 a l r) = Op2 a (elimBi l) (elimBi r)
elimBi !(Neg a)     = Neg  (elimBi a)
elimBi !x           = x

-- | Eliminate Implications
elimImp :: VProp d a -> VProp d a
elimImp !(Op2 Impl a c)     = _or (Neg $ elimImp a) (elimImp c)
elimImp !x@(Op2 BiImpl _ _) = elimImp $ elimBi x
elimImp !(Chc d l r) = Chc d (elimImp l) (elimImp r)
elimImp !(Op2 a l r) = Op2 a (elimImp l) (elimImp r)
elimImp !(Neg a)     = Neg   (elimImp a)
elimImp !x@(Ref _)   = x

---------------------- Choice Functions ----------------------------------------
-- | smart constructor for obj
one :: a -> VProp d a
one = Ref

-- | smart constructor for chc
chc :: d -> VProp d a -> VProp d a -> VProp d a
chc = Chc

-- | Wrapper around engine
prune :: (Ord d) => VProp d b -> VProp d b
prune = pruneTagTree M.empty

-- | Given d config and variational expression remove redundant choices
pruneTagTree :: (Ord d) => Config d -> VProp d b -> VProp d b
pruneTagTree _ (Ref d) = Ref d
pruneTagTree tb (Chc t y n) = case M.lookup t tb of
                             Nothing -> Chc t
                                        (pruneTagTree (M.insert t True tb) y)
                                        (pruneTagTree (M.insert t False tb) n)
                             Just True -> pruneTagTree tb y
                             Just False -> pruneTagTree tb n
pruneTagTree tb (Neg x)      = Neg $ pruneTagTree tb x
pruneTagTree tb (Op2 a l r)  = Op2 a (pruneTagTree tb l) (pruneTagTree tb r)

-- | Count the terms in the expression
numTerms :: VProp d a -> Integer -> Integer
numTerms (Ref _) acc     = succ acc
numTerms (Neg a) acc     = numTerms a acc
numTerms (Op2 _ l r) acc = numTerms l (numTerms r acc)
numTerms Chc{}       acc = succ acc

-- | Count the choices in a tree
numChc :: VProp d a -> Integer
numChc = bifoldr' (\_ acc -> succ acc) (\_ acc -> acc) 0

-- | Depth of the Term tree
depth :: VProp d a -> Integer -> Integer
depth (Ref _) acc = acc
depth (Neg a) acc = depth a (succ acc)
depth (Op2 _ l r) acc = max (depth l (succ acc)) (depth r (succ acc))
depth Chc{}       acc = acc

-- | Given a config and a Variational Prop term select the element out that the
-- config points to
select :: (Ord d) => Config d -> VProp d a -> Maybe (VProp d a)
select _ (Ref a) = Just $ Ref a
select tbs (Chc t y n) = case M.lookup t tbs of
                           Nothing    -> Nothing
                           Just True  -> select tbs y
                           Just False -> select tbs n
select tb (Neg x)      = Neg   <$> select tb x
select tb (Op2 a l r)  = Op2 a <$> select tb l <*> select tb r

-- | And Decomposition, convert choices to propositional terms
andDecomp :: VProp a a -> VProp a a
andDecomp (Chc t l r) = _or
                        (_and (Ref t)       (andDecomp l))
                        (_and (Neg $ Ref t) (andDecomp r))
andDecomp (Ref x)     = Ref x
andDecomp (Neg x)     = Neg (andDecomp x)
andDecomp (Op2 a l r) = Op2 a (andDecomp l) (andDecomp r)

-- | Given a variational term find all paths for the tree in a flat list
-- TODO cleanup nub and filter calls
paths :: Ord d => VProp d a -> [Config d]
paths = nub . filter (not . M.null) . go
  where
    go (Chc d l r) = do
      summaryl <- go l
      summaryr <- go r
      [M.insert d True summaryl, M.insert d False summaryr]
    go (Neg x) = go x
    go (Op2 _ l r)  = go l ++ go r
    go _ = [M.empty]

-- | Given a tag tree, fmap over the tree with respect to a config
replace :: Ord d => Config d -> a -> VProp d (Maybe a) -> VProp d (Maybe a)
replace _    v (Ref _) = Ref $ Just v
replace conf v (Chc d l r) =
  case M.lookup d conf of
    Nothing    -> Chc d (replace conf v l) (replace conf v r)
    Just True  -> Chc d (replace conf v l) r
    Just False -> Chc d l (replace conf v r)
replace conf v (Neg x) = Neg $ replace conf v x
replace conf v (Op2 a l r) = Op2 a (replace conf v l) (replace conf v r)

-- | helper function used to create seed value for fold just once
_recompile :: Ord d => Config d -> a -> VProp d (Maybe a)
_recompile conf = go (M.toList conf)
  where
    go :: [(d, Bool)] -> a -> VProp d (Maybe a)
    go [] val' = Ref . Just $ val'
    go ((d, b):cs) val'
          | b = Chc d (go cs val') (Ref Nothing)
          | otherwise = Chc d (Ref Nothing) (go cs val')

-- | Given a list of configs with associated values, remake the tag tree by
-- folding over the config list
recompile :: Ord d => [(Config d, a)] -> Maybe (VProp d a)
recompile [] = Nothing
recompile xs = sequence $ go (tail xs') (_recompile conf val)
  where
    xs' = reverse $ sortOn (M.size . fst) xs
    (conf, val) = head xs'
    go :: Ord d => [(Config d, a)] -> VProp d (Maybe a) -> VProp d (Maybe a)
    go []          acc = acc
    go ((c, v):cs) acc = go cs $ replace c v acc

-- toProp :: VProp d a -> P.Prop
-- toProp (Ref f) = P.Ref $ P.Feature f

-- data VProp d a = Ref !a                            -- ^ A literal result of a choice
--                | Neg !(VProp d a)                  -- ^ Negation
--                | Op2 Op2 !(VProp d a) !(VProp d a) -- ^ Binary Terms
--                | Chc d  !(VProp d a) !(VProp d a)  -- ^ A Choice in Dimension d
--             deriving (Eq, Generic)
