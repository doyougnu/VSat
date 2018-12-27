import           System.Environment
import           Test.Tasty
import           Test.Tasty.QuickCheck

import qualified CaseStudy.Auto.Parser.Test as CAP
import qualified Run.Test as R
import           VProp.Core.Test
import           VProp.Json.Test

main :: IO ()
main = do
  setEnv "TASTY_QUICKCHECK_TESTS" "3000"
  defaultMain tests
  where tests = testGroup "All" [ -- jsonProperties
<<<<<<< HEAD
                                 -- R.unitTests
          R.specTests
                                , CAP.unitTests
=======
                                 -- unitTests
                                CAP.unitTests
>>>>>>> 988defcf0c6cc869c1ae92410a1dd4dad5412caf
                                -- coreProperties
                                -- ,
                                -- , runProperties
                                ]
