module Main where

import Run
import VProp             (numTerms, numChc, depth, genVProp)
import Criterion.Main

main :: IO ()
main = do

        props <- sequence . take 5 $ repeat genVProp
        defaultMain $
          [ bgroup "andDecomp" $! b <$> props
          ]
          where
            b x = bench ("NumTerms: " ++ (show $ numTerms x) ++ "\n" ++
                          "NumChc: " ++ (show $ numChc x) ++ "\n" ++
                          "Depth: " ++ (show $ depth x)) $! nfIO (runEnv False x)


        -- defaultMain $
        --   [ bgroup "andDecomp" $! b <$> props
        --   ]
        --   where
        --     runAll opts = flip runEnv Opts {baseline=opts, others=[]} . initAndRun
        --     b x = bench ("NumTerms: " ++ (show $ numTerms x 0) ++ "\n" ++
        --                   "NumChc: " ++ (show $ numChc x) ++ "\n" ++
        --                   "Depth: " ++ (show $ depth x 0)) $! nfIO (runAll True x)
