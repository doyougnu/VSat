module Main where

import Run
import Criterion.Main

main :: IO ()
main = defaultMain $
  [ bgroup "test"
    [ bench "1" $ whnf runAll p1
    ]
  ]
  where runAll = runEnv . initEnv
