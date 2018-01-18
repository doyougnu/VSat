module Main where

import Run
import Gen
import Criterion.Main

main :: IO ()
main = do
        props <- sequence $ take 10 $ repeat genVProp
        defaultMain $
          [ bgroup "test" $ (b) <$> props
          ]
          where runAll = runEnv . initAndRun
                b x = bench (show x) $ whnf runAll x
