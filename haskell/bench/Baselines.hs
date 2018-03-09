import Criterion.Main as C

import Run
import VProp (numTerms,numChc,depth,genVProp,VProp)


runner :: Bool -> VProp -> Benchmark
runner bool x = bench ("\nNumTerms: " ++ (show $ numTerms x) ++ "\n" ++
                        "NumChc: " ++ (show $ numChc x) ++ "\n" ++
                        "Depth: " ++ (show $ depth x)) $ C.whnfIO (runEnv bool x)

main :: IO ()
main = do
        props <- sequence . take 5 $ repeat genVProp
        C.defaultMain
          [ C.bgroup "Baselines"
            [ bench "Brute Force" $ C.whnfIO (runEnv False (head props))
            , bench "And Decomposition" $ C.whnfIO (runEnv True (head props))
            ]
          ]
