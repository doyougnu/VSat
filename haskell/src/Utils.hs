module Utils where

import Data.List (foldl1')

-- | Get the fst of a triple
fst' :: (a, b, c) -> a
fst' (a,_,_) = a

-- | Get the fst of a triple
snd' :: (a, b, c) -> b
snd' (_,b,_) = b

-- | Get the fst of a triple
thd' :: (a, b, c) -> c
thd' (_,_,c) = c

onFst :: (a -> d) -> (a,b,c) -> (d,b,c)
onFst f (a,b,c) = (f a, b, c)

onSnd :: (b -> d) -> (a,b,c) -> (a,d,c)
onSnd f (a,b,c) = (a, f b, c)

onThd :: (c -> d) -> (a,b,c) -> (a,b,d)
onThd f (a,b,c) = (a, b, f c)

-- we construct a balanced binary tree given a binary operator. We find the
-- middle point of the list and recur down to leaves.
-- TODO this should become a
-- traversable t, but need to define take and drop for traversable
fromList :: (a -> a -> a) -> [a] -> a
fromList cnstr ps = go m ps
  where
    m = Prelude.length ps
    go 0 xs = go (Prelude.length xs) xs
    go _ [x]  = x
    go _ (x:y:[]) = cnstr x y
    go !n !xs = cnstr (go n' xs') (go n' ys)
      where
        n' = (n `div` 2)
        xs' = Prelude.take n' xs
        ys =  if even n
              then Prelude.drop n' xs
              else Prelude.drop (n' + 1) xs

fromList' :: (a -> a -> a) -> [a] -> a
fromList' = foldl1'

-- | given a list of stuff, generate a list of those things tagged with every
-- possible boolean combination. i.e. booleanCombinations [1..3] = [[(1, True),
-- (2, True) (3, True)], [(1, True), (2, True), (3, False)] ...]
booleanCombinations :: [a] -> [[(a, Bool)]]
booleanCombinations [] = []
  -- this singleton case is super important, if missed the fmap will only ever
  -- return empty list and the recursion won't consider the values
booleanCombinations [x] = [[(x, True)], [(x, False)]]
booleanCombinations (d:dims) =
  fmap ((d, True) :) cs ++ fmap ((d, False) :) cs
  where cs = booleanCombinations dims
