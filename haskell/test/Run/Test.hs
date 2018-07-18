module Run.Test where

import Test.Tasty
import Test.Tasty.QuickCheck as QC

import VProp.Types
import VProp.Core
import VProp.SBV
import VProp.Gen
import Run

-- runProperties :: TestTree
-- runProperties = testGroup "Run Properties" [qcProps]


-- TODO figure out how to do this with monads
-- qcProps = testGroup "QuickChecked Properties"
--   [ QC.testProperty "And Decomposition solution should always be in the set of complete solutions" $
--     \x -> runAD [] (x :: VProp String String) `elem` runVSMT [] x
--   ]
