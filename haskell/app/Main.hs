module Main where

import Run
import Gen
-- import VProp
-- import Criterion.Main

main :: IO ()
main = do
        props <- sequence . take 1 $! repeat genVProp
        print $ head props
        xs <- (flip evalEnv) Opts{baseline=True, others=[]} . initAndRun $ head props
        print xs
        -- return ()


        -- defaultMain $
        --   [ bgroup "andDecomp" $! b <$> props
        --   ]
        --   where runAll opts = flip runEnv Opts {baseline=opts, others=[]} . initAndRun
        --         b x = bench ("NumTerms: " ++ (show $ numTerms x 0) ++ "\n" ++
        --                     "NumChc: " ++ (show $ numChc x) ++ "\n" ++
        --                     "Depth: " ++ (show $ depth x 0)) $! nfIO (runAll True x)
