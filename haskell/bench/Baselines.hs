import Criterion.Main as C

import Run
import VProp (genVProp)


main :: IO ()
main = do
        prop <- genVProp
        print prop
        C.defaultMain
          [ C.bgroup "Baselines"
            [ bench "Brute Force" $ C.whnfIO (runEnv False prop)
            , bench "And Decomposition" $ C.whnfIO (runEnv True prop)
            ]
          ]
