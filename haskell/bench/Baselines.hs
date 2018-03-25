import Criterion.Main as C

import Run
import VProp (genVProp)

-- run with $ stack bench --benchmark-arguments "--output <benchmark-file>.html"
main :: IO ()
main = do
        prop <- genVProp
        print prop
        C.defaultMain
          [ C.bgroup "Baselines"
            [ bench "Brute Force" $ C.nfIO (runEnv True False False [] prop)
            , bench "And Decomposition" $ C.nfIO (runEnv True True False [] prop)
            , bench "Variational Solve" $ C.nfIO (runEnv False False False [] prop)
            ]
          ]
