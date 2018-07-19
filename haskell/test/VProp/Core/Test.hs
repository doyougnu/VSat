module VProp.Core.Test where

import Test.Tasty
import Test.Tasty.QuickCheck as QC
import qualified Data.Set as Set
import Data.List  (group, genericLength)

import VProp.Types
import VProp.Core
import VProp.SBV
import VProp.Gen

coreProperties :: TestTree
coreProperties = testGroup "Core Properties" [qcProps]

freq :: (Integral b, Eq a) => [a] -> [([a], b)]
freq = fmap (\x -> (x , genericLength x)) . group

qcProps = testGroup "QuickChecked Properties"
  [ QC.testProperty "And Decomposition removes all choices" $
    \x -> True == (isPlain $ andDecomp (x :: VProp String String) dimName)

  , QC.testProperty "Number of Terms == num Chc + num Plains" $
    \x -> numTerms (x :: VProp String String) == (numChc x) + (numPlain x)

  , QC.testProperty "If a prop is plain then there are no choices" $
    \x -> if isPlain (x :: VProp String String)
          then (null $ dimensions x) == True
          else (null $ dimensions x) == False

  , QC.testProperty "Destructors have no duplicates: dimensions" $
    \x -> (length . Set.toList $ dimensions (x :: VProp String String))
          ==
          (foldr (\x acc -> acc + snd x) 0 . freq . Set.toList $ dimensions x)

  , QC.testProperty "Destructors have no duplicates: vars" $
    \x -> (length . Set.toList $ vars (x :: VProp String String))
          ==
          (foldr (\x acc -> acc + snd x) 0 . freq . Set.toList $ vars x)

  , QC.testProperty "Destructors have no duplicates: ivars" $
    \x -> (length . Set.toList $ ivars (x :: VProp String String))
          ==
          (foldr (\x acc -> acc + snd x) 0 . freq . Set.toList $ ivars x)

  , QC.testProperty "Destructors have no duplicates: ivars" $
    \x -> (length . Set.toList $ ivars (x :: VProp String String))
          ==
          (foldr (\x acc -> acc + snd x) 0 . freq . Set.toList $ ivars x)
  ]
