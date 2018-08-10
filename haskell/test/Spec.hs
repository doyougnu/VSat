import Test.Tasty
import Test.Tasty.QuickCheck

import VProp.Core.Test
import Run.Test

main :: IO ()
main = do defaultMain tests
  where tests = testGroup "All" [ runProperties
                                 , unitTests
                                -- coreProperties
                                -- , runProperties
                                ]
