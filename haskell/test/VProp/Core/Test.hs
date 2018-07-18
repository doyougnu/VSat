module VProp.Core.Test where

import Test.Tasty
import Test.Tasty.QuickCheck as QC
import Data.Bifunctor (bimap)


import VProp.Types
import VProp.Core
import VProp.SBV
import VProp.Gen

coreProperties :: TestTree
coreProperties = testGroup "Properties" [qcProps]

qcProps = testGroup "QuickChecked Properties"
  [ QC.testProperty "And Decomposition removes all choices" $
    \x ->
      True == (isPlain $
                   andDecomp (x :: VProp String String) dimName)
  ]
