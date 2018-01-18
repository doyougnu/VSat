module Main where

import Run
import Gen
import VProp
import Criterion.Main

main :: IO ()
main = do
        props <- sequence . take 10 $ repeat genVProp
        defaultMain $
          [ bgroup "andDecomp" $ (b) <$> props
          ]
          where runAll = runEnv . initAndRun
                b x = bench ("NumTerms: " ++ (show $ numTerms x 0) ++ "\n" ++
                            "NumChc: " ++ (show $ numChc x) ++ "\n" ++
                            "Depth: " ++ (show $ depth x 0)) $ whnf runAll x
