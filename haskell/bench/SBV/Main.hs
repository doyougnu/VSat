module Main where

import Control.Monad
-- import Control.DeepSeq (force)
import Data.SBV
import Data.SBV.Control
import Data.SBV.Internals (cvToBool)
import Data.String (fromString)
import Data.Map

import           Criterion.Main

-- | Create a n number of variables, conjunct them all and solve.
-- Parameterized over a solver
batchTest :: SMTConfig -> Int -> IO SatResult
batchTest solver n = satWith solver $ do bs <- mapM (const free_) [1..n]
                                         b <- foldM go sTrue bs
                                         constrain b
                                         return b
  where go x acc = return $ x .&& acc

-- | Simple query mode batchTest, create 3 boolean variables, do a constraint, push
-- into query mode constrain, get another result, pop, and repeat. Then return a
-- list of model dictionaries
testQuery :: SMTConfig -> IO [Map String Bool]
testQuery solver = runSMTWith solver $
  do x <- sBool "x"
     y <- sBool "y"
     z <- sBool "z"

     constrain $ x .&& y
     query $ do

       m1 <- inNewAssertionStack $ do constrain $ z .<+> y
                                      getResult

       m2 <- inNewAssertionStack $ do constrain $ z .|| x
                                      constrain $ y .<=> x
                                      getResult
       return [m1,m2]

-- | Helper function to get an SMT result and construct the model dictionary by
-- converting internal CV values to Bools
getResult :: Query (Map String Bool)
getResult =
  do model <- getSMTResult
     case model of
       m@(Satisfiable _ _)       -> return . toResMap . getModelDictionary $! m
       _                         -> return mempty
  where
    toResMap = foldMapWithKey (\k a -> singleton k (cvToBool a))

-- | helper function for batchTests
batchBench solver = [ bench "10" . nfIO   $ batchTest solver 10 ]

-- | helper for query tests
queryBench solver = [ bench "" . nfIO   $ testQuery solver{verbose=True} ]

main = defaultMain [ -- bgroup "Z3"    $ batchBench z3
                   -- , bgroup "Yices" $ batchBench yices
                   -- , bgroup "Abc"   $ batchBench abc
                   -- , bgroup "CVC4"  $ batchBench cvc4
                   -- , bgroup "Boolector"  $ batchBench boolector

                   -- query tests
                     bgroup "Z3"    $ queryBench z3
                   -- , bgroup "Yices" $ queryBench yices
                   , bgroup "Abc"   $ queryBench abc
                   , bgroup "CVC4"  $ queryBench cvc4
                   ]
