module TagTree where

import Data.Map (Map)
import Data.List (transpose)
import qualified Data.Map as M
import qualified Data.Set as S
import Control.Monad
import Data.Maybe (isJust, isNothing)

type Tag = Integer

type Config = [(Tag, Bool)]

data V a = Obj a
         | Chc Tag (V a) (V a)
         deriving (Eq, Ord)

-- | smart constructor for obj
one :: a -> V a
one = Obj

-- | smart constructor for chc
chc :: Tag -> V a -> V a -> V a
chc t y n = Chc t y n

-- | Pull the tag out of a Variational term
tag :: (Integral a) => V a -> Maybe Tag
tag (Chc t _ _) = Just t
tag _           = Nothing

-- | Given a variation term, if it is an object, get the object
getObj :: V a -> Maybe a
getObj (Obj a) = Just a
getObj _ = Nothing

-- | Given a variational expression, return true if its an object
isObj :: V a -> Bool
isObj = isJust . getObj

-- | Given a variational expression, return true if its an choice
isChc :: V a -> Bool
isChc = not . isObj

-- | Wrapper around engine
prune :: V a -> V a
prune = prune_tagtree []

-- | Given a config and variational expression remove redundant choices
prune_tagtree :: Config -> V a -> V a
prune_tagtree _ (Obj a) = Obj a
prune_tagtree tb (Chc t y n) = case lookup t tb of
                             Nothing -> Chc t
                                        (prune_tagtree ((t,True):tb) y)
                                        (prune_tagtree ((t,False):tb) n)
                             Just True -> prune_tagtree tb y
                             Just False -> prune_tagtree tb n

-- | Given a configuration and a variational expression perform a selection
select :: Config -> V a -> Maybe a
select _ (Obj a) = Just a
select tbs (Chc t y n) =
  case lookup t tbs of
    Nothing   -> Nothing
    Just True -> select tbs y
    Just False -> select tbs n

instance Functor V where
  fmap f (Obj a) = Obj (f a)
  fmap f (Chc t y n) = Chc t (fmap f y) (fmap f n)

instance Applicative V where
  pure = one
  (Obj f) <*> (Obj e) = Obj $ f e
  f@(Obj f') <*> (Chc t l r) = (Chc t (f <*> l) (f <*> r))
  (Chc t fl fr) <*> a@(Obj e) = Chc t (fl <*> a) (fr <*> a)
  (Chc t fl fr) <*> (Chc t' el er) = (Chc t
                                      (Chc t' (fl <*> el) (fl <*> er))
                                      (Chc t' (fr <*> el) (fr <*> er)))

instance Monad V where
  return  = Obj
  Obj a >>= f = f a
  Chc t y n >>= f = Chc t (y >>= f)(n >>= f)

instance Show a => Show (V a) where
  show (Obj a)      = show a
  show (Chc t y n)   = (show t) ++ "<" ++ show y ++ ", " ++ show n ++ ">"
