module Utils where

import           Control.DeepSeq (NFData)
import           System.CPUTime
import           System.Timeout

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

time :: (NFData a, Fractional d) =>  IO a -> IO (d, a)
time !a = do
  start <- getCPUTime
  v <- a
  end' <- timeout 300000000 (v `seq` getCPUTime)
  let end = maybe (300 * 10^12) id end'
      diff = (fromIntegral (end - start)) / (10 ^ 12)
  return (diff, v)
