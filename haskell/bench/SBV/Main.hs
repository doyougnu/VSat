import Control.Monad
import Control.DeepSeq (force)
import Data.SBV

import           Criterion.Main

bad :: Int -> IO SatResult
bad xs = sat $ do bs <- mapM (const free_) [1..xs]
                  foldM go sTrue bs
  where go !x !acc = let !s = x .&& acc
                   in do constrain s
                         return s

good :: Int -> IO SatResult
good xs = sat $ do bs <- mapM (const free_) [1..xs]
                   b <- foldM go sTrue bs
                   constrain b
                   return b
  where go x acc = return $ x .&& acc

main = defaultMain [ bgroup "good" [ bench "10" . nfIO $ good 10
                                   , bench "100" . nfIO $ good 100
                                   , bench "1000" . nfIO $ good 1000
                                   , bench "10000" . nfIO $ good 10000
                                   ]
                   , bgroup "bad" [ bench "10" . nfIO $ bad 10
                                  , bench "100" . nfIO $ bad 100
                                  , bench "1000" . nfIO $ bad 1000
                                  , bench "10000" . nfIO $ bad 10000
                                  ]
                      ]
