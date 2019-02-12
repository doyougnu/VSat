import           System.Environment
import           Test.Tasty
import           Test.Tasty.QuickCheck

import qualified CaseStudy.Auto.Parser.Test as CAP
import qualified Run.Test as R
import qualified VProp.Core.Test as VC
import           VProp.Json.Test

main :: IO ()
main = do
  setEnv "TASTY_QUICKCHECK_TESTS" "3000"
  defaultMain tests
  where tests = testGroup "All" [ -- jsonProperties
                                 R.unitTests
                                -- , R.specTests
                                -- , CAP.unitTests
                                -- R.runProperties
          -- VC.qcProps
                                ]
