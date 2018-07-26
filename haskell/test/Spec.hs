import Test.Tasty
import Test.Tasty.QuickCheck

import VProp.Core.Test
import Run.Test

main :: IO ()
main = do defaultMain runProperties
  where tests = testGroup "All" [ runProperties
                                , coreProperties
                                ]
