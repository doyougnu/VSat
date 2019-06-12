module VProp.Core.Test where

import           Data.List             (genericLength, group)
import qualified Data.Set              as Set
import qualified Data.Text             as T
import           Test.Tasty
import           Test.Tasty.QuickCheck as QC

import           Run
import           VProp.Core
import           VProp.Gen
import           VProp.SBV
import           VProp.Types

import Debug.Trace (trace)

coreProperties :: TestTree
coreProperties = testGroup "Core Properties" [qcProps]

freq :: (Integral b, Eq a) => [a] -> [([a], b)]
freq = fmap (\x -> (x , genericLength x)) . group

qcProps = testGroup "QuickChecked Properties"
  [ QC.testProperty "And Decomposition removes all choices" $
    \x -> True == (isPlain $ andDecomp (x :: VProp Var Var Var) dimName)

  , QC.testProperty "Number of Terms == num Chc + num Plains" $
    \x -> numTerms (x :: VProp Var Var Var) == (numChc x) + (numPlain x)

  , QC.testProperty "If a prop is plain then there are no choices" $
    \x -> if isPlain (x :: VProp Var Var Var)
          then (null $ dimensions x) == True
          else (null $ dimensions x) == False

  , QC.testProperty "Destructors have no duplicates: dimensions" $
    \x -> (length . Set.toList $ dimensions (x :: VProp Var Var Var))
          ==
          (foldr (\x acc -> acc + snd x) 0 . freq . Set.toList $ dimensions x)

  , QC.testProperty "Destructors have no duplicates: vars" $
    \x -> (length . Set.toList $ vars (x :: VProp Var Var Var))
          ==
          (foldr (\x acc -> acc + snd x) 0 . freq . Set.toList $ vars x)

  , QC.testProperty "Destructors have no duplicates: ivars" $
    \x -> (length . Set.toList $ ivars (x :: VProp Var Var Var))
          ==
          (foldr (\x acc -> acc + snd x) 0 . freq . Set.toList $ ivars x)

  , QC.testProperty "Destructors have no duplicates: ivars" $
    \x -> (length . Set.toList $ ivars (x :: VProp Var Var Var))
          ==
          (foldr (\x acc -> acc + snd x) 0 . freq . Set.toList $ ivars x)

  , QC.testProperty "And Decomposition does not affect plain props" $
    \x -> isPlain x QC.==> andDecomp x dimName == (x :: VProp Var Var Var)
  , QC.testProperty "Config generation only generates complete configs" selectionToPlain
  ]



selectionToPlain :: ReadableProp Var -> Property
selectionToPlain x = isVariational x QC.==>
  do
    let dims = dimensions' x
        confs = choices x
        check y = (length dims) == (length y)
    -- trace ("\n--------------\ndims") $
    --   trace (show dims) $
    --   trace ("\n--------------\nconfs") $
    --   trace (show confs) $
    --   trace ("\n--------------\n") $
    all (check) confs
