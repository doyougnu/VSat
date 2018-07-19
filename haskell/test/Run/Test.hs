module Run.Test where

import Test.Tasty
import Test.Tasty.QuickCheck as QC
import Test.QuickCheck.Monadic as QCM

import VProp.Types
import VProp.Core
import VProp.SBV
import VProp.Gen
import Run

runProperties :: TestTree
runProperties = testGroup "Run Properties" [qcProps]

qcProps = testGroup "QuickChecked Properties" []

andDecomp_correct x = QCM.monadicIO $
  do a <- QCM.run $ runAD [] (x :: VProp String String)
     b <- QCM.run $ runAD [] x
     assert ((head a) == (head b))
