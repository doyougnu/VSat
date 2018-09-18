module Utils where

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
