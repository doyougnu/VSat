import           System.Environment
import           Test.Tasty
import           Test.Tasty.QuickCheck

import qualified CaseStudy.Auto.Parser.Test as CAP
import qualified CaseStudy.Auto.Run.Test as CAR
import qualified Run.Test as R
import qualified Opts.Test as O
import qualified VProp.Core.Test as VC
import           VProp.Json.Test

main :: IO ()
main = do
  setEnv "TASTY_QUICKCHECK_TESTS" "3000"
  defaultMain tests
  where tests = testGroup "All" [ -- jsonProperties
                                 -- R.unitTests
                                -- , R.specTests
                                -- , CAP.unitTests
                                -- CAR.unitTests
                                -- , CAP.unitTests
                                R.runProperties
                                -- O.properties
          -- VC.qcProps
                                ]
