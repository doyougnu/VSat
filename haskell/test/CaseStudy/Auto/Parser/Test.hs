module CaseStudy.Auto.Parser.Test where

import           Data.Either           (isRight)
import           Data.Text             (Text)
import           Test.Tasty
import qualified Test.Tasty.HUnit      as H

import           CaseStudy.Auto.Lang
import           CaseStudy.Auto.Parser (langParser)
import           Text.Megaparsec       (parse)

unitTests :: TestTree
unitTests = testGroup "Parser unit tests" $
  [
    aFeature
  , aSimplEvoCtx
  , aConjEvoContext
  , twoImplClause
  , twoImplClause2
  , iffOneAndOnly
  , aConjEvoContext2
  , iffWithOrs
  , iffSingle
  , andNots
  , aSimplEvoCtx2
  , aConjEvoWithNot
  , conjEvoiffSingle
  , aSimplEvoCtx3
  , aConjEvoContext3
  , aSimplEvoCtx4
  , arithAdd
  , xOrList
  ]

test :: [Text]
test = [
-- 0
  "feature[_9aa0af2e-d9da-4de1-a6d2-c36aa8950af1]"
-- 1
       , "(context[_evolution-context] >= 0 and context[_evolution-context] < 1) impl (feature[_9aa0af2e-d9da-4de1-a6d2-c36aa8950af1] impl feature[_c8702f9c-5955-41d6-b85b-7a6fb93f1862])"
-- 2
       , "(context[_evolution-context] >= 0 and context[_evolution-context] < 1) impl ((feature[_6250b555-dc62-43bd-a42b-7bdecc7230bd] or feature[_c8702f9c-5955-41d6-b85b-7a6fb93f1862]) impl feature[_9aa0af2e-d9da-4de1-a6d2-c36aa8950af1])"
-- 3
       , "(context[_evolution-context] >= 1) impl (feature[_9aa0af2e-d9da-4de1-a6d2-c36aa8950af1] impl (feature[_6250b555-dc62-43bd-a42b-7bdecc7230bd] and feature[_c8702f9c-5955-41d6-b85b-7a6fb93f1862]))"
-- 4
       , "(context[_evolution-context] >= 1) impl ((feature[_6250b555-dc62-43bd-a42b-7bdecc7230bd] or feature[_c8702f9c-5955-41d6-b85b-7a6fb93f1862]) impl feature[_9aa0af2e-d9da-4de1-a6d2-c36aa8950af1])"
-- 5
       , "(context[_evolution-context] >= 0 and context[_evolution-context] < 1) impl (feature[_6250b555-dc62-43bd-a42b-7bdecc7230bd] iff oneonly [ feature[_53bf0440-415c-430d-b3d4-bc9c30a64ab1], feature[_4a716d46-f62d-4bee-85b9-ba37b51d4f84], feature[_a84bbb49-bfe1-4e03-ad11-1765676e4225] ])"
-- 6
       , "(context[_evolution-context] >= 0 and context[_evolution-context] < 1) impl ((feature[_53bf0440-415c-430d-b3d4-bc9c30a64ab1] or feature[_4a716d46-f62d-4bee-85b9-ba37b51d4f84] or feature[_a84bbb49-bfe1-4e03-ad11-1765676e4225]) impl feature[_6250b555-dc62-43bd-a42b-7bdecc7230bd])"
-- 7
       , "(context[_evolution-context] >= 1 and context[_evolution-context] < 2) impl (feature[_6250b555-dc62-43bd-a42b-7bdecc7230bd] iff (feature[_53bf0440-415c-430d-b3d4-bc9c30a64ab1] or feature[_4a716d46-f62d-4bee-85b9-ba37b51d4f84] or feature[_a84bbb49-bfe1-4e03-ad11-1765676e4225]))"
-- 8
       , "(context[_evolution-context] >= 2) impl (feature[_6250b555-dc62-43bd-a42b-7bdecc7230bd] iff (feature[_53bf0440-415c-430d-b3d4-bc9c30a64ab1] or feature[_4a716d46-f62d-4bee-85b9-ba37b51d4f84]))"
-- 9
       , "(context[_evolution-context] < 0) impl ( not feature[_6250b555-dc62-43bd-a42b-7bdecc7230bd] and  not feature[_c8702f9c-5955-41d6-b85b-7a6fb93f1862] and  not feature[_53bf0440-415c-430d-b3d4-bc9c30a64ab1] and  not feature[_4a716d46-f62d-4bee-85b9-ba37b51d4f84] and  not feature[_a84bbb49-bfe1-4e03-ad11-1765676e4225])"
       , "(context[_evolution-context] >= 1) impl ( not feature[_a84bbb49-bfe1-4e03-ad11-1765676e4225])"
       , "(context[_evolution-context] >= 0 and context[_evolution-context] < 1) impl ( not feature[_6250b555-dc62-43bd-a42b-7bdecc7230bd])"
       , "(context[_evolution-context] >= 1 and context[_evolution-context] < 2) impl (feature[_53bf0440-415c-430d-b3d4-bc9c30a64ab1] iff feature[_a84bbb49-bfe1-4e03-ad11-1765676e4225])"
       , "(context[_evolution-context] >= 1) impl (feature[_4a716d46-f62d-4bee-85b9-ba37b51d4f84] impl feature[_53bf0440-415c-430d-b3d4-bc9c30a64ab1])"
       , "(context[_evolution-context] >= 1 and context[_evolution-context] < 2) impl ((feature[_c8702f9c-5955-41d6-b85b-7a6fb93f1862] and feature[_9aa0af2e-d9da-4de1-a6d2-c36aa8950af1]) impl feature[_6250b555-dc62-43bd-a42b-7bdecc7230bd])"
       , "(context[_evolution-context] >= 2) impl ((feature[_c8702f9c-5955-41d6-b85b-7a6fb93f1862] or feature[_9aa0af2e-d9da-4de1-a6d2-c36aa8950af1]) impl feature[_6250b555-dc62-43bd-a42b-7bdecc7230bd])"
       ]

aFeature         = H.testCase "A singleton of a bool ref" aFeature'
aSimplEvoCtx     = H.testCase "a clause with an evo context" aSimpleEvoCtx'
aConjEvoContext  = H.testCase "A term with an _and_ in evo context " aConjEvoContext'
twoImplClause    = H.testCase "A term with a chained implication" twoImplClause'
twoImplClause2   = H.testCase "A term with a chained implication" twoImplClause2'
iffOneAndOnly    = H.testCase "A term with an iff and oneAndOnly operator" iffOneAndOnly'
aConjEvoContext2 = H.testCase "a term with an _and_ in evo context" aConjEvoContext2'
iffWithOrs       = H.testCase "A term with iff and chained Ors" iffWithOrs'
iffSingle        = H.testCase "A term with just an iff" iffSingle'
andNots          = H.testCase "A clause with chained _not_ _and_ clauses" andNots'
aSimplEvoCtx2    = H.testCase "a clause with an evo context" aSimpleEvoCtx2'
aConjEvoWithNot  = H.testCase "a clause with conjuctive evo contexts and a not" aConjEvoWithNot'
conjEvoiffSingle = H.testCase "A term with conjuction evo ctxs and a iff" conjEvoiffSingle'
aSimplEvoCtx3    = H.testCase "a clause with an evo context" aSimplEvoCtx3'
aConjEvoContext3 = H.testCase "a term with an _and_ in evo context" aConjEvoContext3'
aSimplEvoCtx4    = H.testCase "a clause with an evo context" aSimpleEvoCtx4'
-- arithAdd         = H.testCase "addition with features parses" arithAdd'
xOrList          = H.testCase "Xor is parsed returning a list of possible features" xOrList'

-- | given a string, parse the string and check that the parser succeeded
parseSucceedsGen = H.assertBool "Failed to Parse" . isRight . parse langParser ""

parseSucceedsGen' :: Text -> H.Assertion
parseSucceedsGen' xs =
  do let res = parse langParser "" xs
     print res
     H.assertBool "Failed to Parse" $ isRight res

aFeature'         = parseSucceedsGen $ test !! 0
aSimpleEvoCtx'    = parseSucceedsGen $ test !! 1
aConjEvoContext'  = parseSucceedsGen $ test !! 2
twoImplClause'    = parseSucceedsGen $ test !! 3
twoImplClause2'   = parseSucceedsGen $ test !! 4
iffOneAndOnly'    = parseSucceedsGen $ test !! 5
aConjEvoContext2' = parseSucceedsGen $ test !! 6
iffWithOrs'       = parseSucceedsGen $ test !! 7
iffSingle'        = parseSucceedsGen $ test !! 8
andNots'          = parseSucceedsGen $ test !! 9
aSimpleEvoCtx2'   = parseSucceedsGen $ test !! 9
aConjEvoWithNot'  = parseSucceedsGen $ test !! 10
conjEvoiffSingle' = parseSucceedsGen $ test !! 11
aSimplEvoCtx3'    = parseSucceedsGen $ test !! 12
aConjEvoContext3' = parseSucceedsGen $ test !! 13
aSimpleEvoCtx4'   = parseSucceedsGen $ test !! 14

-- arithAdd' = parseSucceedsGen p
--   -- this is a type error?!?!?
--   -- this was confirmed to be a type error
--   where p = "(feature[_84928c30-724e-4e73-b9fb-733518d0e3c6] = 1) = (feature[_505abd5a-edac-4ea8-89fa-79e7297e5c3e] + feature[_c6c50923-72d1-4d46-a429-0839a91df6f2])"

xOrList' = parseSucceedsGen' p
  where p = "feature[_2ad480a4-eb1b-4846-bb97-00595e23faf3] iff oneonly [ feature[_6838428c-3d7c-4f10-8388-b1bdcecc421a], feature[_6ca297a5-8fd5-43bd-999c-d816780aea28], feature[_32070598-98a8-4072-b248-f589d9a95cc6], feature[_1dce285e-f324-4a7c-8399-0e899ecd4d23], feature[_e82d1eba-af04-4fd3-b854-daaef35ba028], feature[_fd07cd5c-29f2-4a44-a29c-38e392e71866], feature[_6dbe4f62-6eda-4365-864c-b3879045c0f2], feature[_ceb20bc9-03b8-43b6-ad70-32efb481f4c7], feature[_691b1794-b2f8-43c0-abc9-b2dc90799553], feature[_8ad64f6a-931c-4d68-99b1-d89761ab9581] ]"
