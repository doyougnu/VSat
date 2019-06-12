module Opts.Test where

import Test.Tasty
import qualified Test.Tasty.HUnit as H
import qualified Test.Tasty.QuickCheck as QC
import qualified Test.QuickCheck.Monadic as QCM
import Data.SBV ( SatResult(..)
                , SMTResult(..)
                , ThmResult(..)
                , SMTConfig(..)
                , SMTSolver(..)
                , Solver(..))
import Data.SBV.Internals (showModel, SMTModel(..))
import Control.Monad.Trans (liftIO)

import VProp.Types hiding (EQ)
import VProp.Core
import VProp.Gen
import Opts

properties :: TestTree
properties = testGroup "Optimization Properties"
  [ atomizationMinimizes

  ]

optsUnitTest :: TestTree
optsUnitTest = testGroup "Optimizations Unit Tests" $ []
  -- [ atomTst
  -- , atomTst2
  -- , atomTst3
  -- , driveTst
  -- ]

atomizationMinimizes =
  QC.testProperty "atomization increases plain terms" atomizationMinimizes'

atomizationMinimizes' x = onlyBools (x :: VarProp) QC.==>
                          numPlain x >= (numPlain $ atomize x)

-- atomTst = H.testCase "Atomization drives choices as far down as possible" $
--   do prop <- genVProp :: IO (VProp Var Var Var)
--      print "--------------\n"
--      print prop
--      print $ atomize prop
--      print "--------------\n"
--      H.assertBool "Returning true just for side effect" True

-- atomTst = H.testCase "Atomization drives choices as far down as possible" $
--      H.assertBool "TODO: This should fail because of the nested +" $ isNormalForm prop
--   where
--     prop :: VProp Var Var Var
--     prop = true &&& bChc "AA"
--            (1 + iChc "BB" (1 + 2) 2 .< (5 ::  VIExpr Var Var))
--            (2 + iChc "CC" 3 4 .> (5 ::  VIExpr Var Var))


-- atomTst2 = H.testCase "The most simple case" $
--      H.assertBool "Passing because Chcs only consist of data" $ isNormalForm prop
--   where
--     prop :: VProp Var Var Var
--     prop = true &&& bChc "AA"
--            (1 + iChc "BB" 1 2 .< (5 ::  VIExpr Var Var))
--            (2 + iChc "CC" 3 4 .> (5 ::  VIExpr Var Var))

-- atomTst3 = H.testCase "Failure upon nested boolean operators" $
--      H.assertBool "TODO: assert this fails because of nested |||" $ isNormalForm prop
--   where
--     prop :: VProp Var Var Var
--     prop = true &&& bChc "AA"
--            (false ||| iChc "BB" 1 2 .< (5 ::  VIExpr Var Var))
--            (2 + iChc "CC" 3 4 .> (5 ::  VIExpr Var Var))

-- driveTst = H.testCase "Choices are driven down over like operators" $
--            atomize prop `compare` resProp H.@?= EQ
--   where prop :: VProp Var Var Var
--         prop = (5 ::  VIExpr Var Var) .>= iChc "A" (1 + 2) (3 + 4)
--         resProp = (5 :: VIExpr Var Var) .>= (iChc "A" 1 3) + (iChc "A" 2 4)
