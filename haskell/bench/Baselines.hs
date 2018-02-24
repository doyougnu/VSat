import Criterion.Main as C
-- import DeepSeq()
import Run
import Gen (genVProp)
import VProp


-- runner :: (Integral a, Ord d, Show a, Show d, Control.DeepSeq.NFData d) =>
--                 Bool -> VProp d a -> Benchmark
runner bool x = bench ("NumTerms: " ++ (show $ numTerms x 0) ++ "\n" ++
                        "NumChc: " ++ (show $ numChc x) ++ "\n" ++
                        "Depth: " ++ (show $ depth x 0)) $! C.whnfIO (runEnv bool x)

main :: IO ()
main = do
        props <- sequence . take 5 $ repeat genVProp
        defaultMain $
          [ bgroup "Brute Force Baseline \n" $ runner False <$> props
          -- , bgroup "And Decomposition Baseline \n" $ runner True <$> props
          ]
-- qsort1 :: (Ord a) => [a] -> [a]
-- qsort1 [] = []
-- qsort1 (x:xs) =
--   qsort1 [y | y <- xs, y < x] ++
--   [y | y <- xs, y == x] ++
--   qsort1 [y | y <- xs, y > x]

-- main :: IO ()
-- main = C.defaultMain [
--   C.bgroup "qsort1" [ C.bench "1000" $ C.whnf qsort1 [1..1000 :: Int]
--                     , C.bench "10000" $ C.whnf qsort1 [1..2000 :: Int]
--                     ]
--   ]
