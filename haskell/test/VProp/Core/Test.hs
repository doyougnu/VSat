module VProp.Core.Test where

import Test.Tasty
import Test.Tasty.QuickCheck as QC

import VProp.Types
import VProp.Core
import VProp.SBV
import VProp.Gen

coreProperties :: TestTree
coreProperties = testGroup "Core Properties" [qcProps]

qcProps = testGroup "QuickChecked Properties"
  [ QC.testProperty "And Decomposition removes all choices" $
    \x -> True == (isPlain $ andDecomp (x :: VProp String String) dimName)

  , QC.testProperty "Number of Terms == num Chc + num Plains" $
    \x -> numTerms (x :: VProp String String) == (numChc x) + (numPlain x)

  , QC.testProperty "If a prop is plain then there are no choices" $
    \x -> if isPlain (x :: VProp String String)
          then (null $ dimensions x) == True
          else (null $ dimensions x) == False

  -- , QC.testProperty "If a prop has onlybool information, then it will have no integer variables" $
  --   \x -> if onlyBools (x :: VProp String String) <+> not (onlyLits x)
  --         then (null $ ivars x) == True
  --         else (null $ ivars x) == False
  ]
