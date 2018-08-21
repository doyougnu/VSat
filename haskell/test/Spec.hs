import Test.Tasty
import Test.Tasty.QuickCheck
import System.Environment

import VProp.Core.Test
import VProp.Json.Test
import Run.Test

main :: IO ()
main = do
  setEnv "TASTY_QUICKCHECK_TESTS" "300"
  defaultMain tests
  where tests = testGroup "All" [ -- jsonProperties
                                 unitTests
                                -- coreProperties
                                -- ,
                                , runProperties
                                ]
