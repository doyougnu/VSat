module Main where

import Run
import Gen

main :: IO ()
main = do

        props <- sequence . take 1 $! repeat genVProp
        print $ head props
        res <- runEnv True (head props)
        -- print res
        -- xs <- (flip evalEnv) Opts{baseline=True, others=[]} . initAndRun $! head props
        -- defaultMain $
        --   [ bgroup "andDecomp" $! b <$> props
        --   ]
        --   where
        --     b x = bench ("NumTerms: " ++ (show $ numTerms x 0) ++ "\n" ++
        --                   "NumChc: " ++ (show $ numChc x) ++ "\n" ++
        --                   "Depth: " ++ (show $ depth x 0)) $! nfIO (runEnv False x)
        print "THE ANSWER: "
        print res
        return ()


        -- defaultMain $
        --   [ bgroup "andDecomp" $! b <$> props
        --   ]
        --   where
        --     runAll opts = flip runEnv Opts {baseline=opts, others=[]} . initAndRun
        --     b x = bench ("NumTerms: " ++ (show $ numTerms x 0) ++ "\n" ++
        --                   "NumChc: " ++ (show $ numChc x) ++ "\n" ++
        --                   "Depth: " ++ (show $ depth x 0)) $! nfIO (runAll True x)
