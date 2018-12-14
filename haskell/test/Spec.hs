import           System.Environment
import           Test.Tasty
import           Test.Tasty.QuickCheck

import qualified CaseStudy.Auto.Parser.Test as CAP
import           Run.Test
import           VProp.Core.Test
import           VProp.Json.Test

main :: IO ()
main = do
  setEnv "TASTY_QUICKCHECK_TESTS" "3000"
  defaultMain tests
  where tests = testGroup "All" [ -- jsonProperties
                                 unitTests
                                , CAP.unitTests
                                -- coreProperties
                                -- ,
                                -- , runProperties
                                ]
