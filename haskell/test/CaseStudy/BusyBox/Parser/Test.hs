module CaseStudy.BusyBox.Parser.Test where

import           Data.Either           (isRight)
import           Data.Text             (Text)
import           Test.Tasty
import qualified Test.Tasty.HUnit      as H

import           CaseStudy.BusyBox.Parser (langParser)
import           Text.Megaparsec       (parse)

unitTests :: TestTree
unitTests = testGroup "Parser unit tests"
  [ one
  , one'
  , two
  , three
  , four
  , five
  , six
  ]

-- | given a string, parse the string and check that the parser succeeded
parseSucceedsGen = H.assertBool "Failed to Parse" . isRight . parse langParser ""

parseSucceedsGen' :: Text -> H.Assertion
parseSucceedsGen' xs =
  do let res = parse langParser "" xs
     print res
     H.assertBool "Failed to Parse" $ isRight res

one   = H.testCase "reference" $ parseSucceedsGen "def(CONFIG_CP_MODULE)"
one'  = H.testCase "reference with numeric" $ parseSucceedsGen "def(CONFIG_RPM2CPIO)"
two   = H.testCase "not ref" $ parseSucceedsGen   "!def(CONFIG_CPIO)"
three = H.testCase "paren not ref" $ parseSucceedsGen "(!def(CONFIG_CPIO))"
four  = H.testCase "compound with not and" $ parseSucceedsGen "(!def(CONFIG_CPIO_MODULE)&!def(CONFIG_RPM))"
five  = H.testCase "compound with and" $ parseSucceedsGen "(!def(CONFIG_CPIO_MODULE)&!def(CONFIG_RPM)&!def(CONFIG_RPM_MODULE)&!def(CONFIG_RPM2CPIO_MODULE)&!def(CONFIG_RPM2CPIO)&!def(CONFIG_CPIO))"
six   = H.testCase "big ass expression" $ parseSucceedsGen "(!def(CONFIG_DESKTOP)&(def(CONFIG_DESKTOP)|!def(CONFIG_FEATURE_FIND_CONTEXT))&(def(CONFIG_DESKTOP)|!def(CONFIG_SELINUX))&(def(CONFIG_FEATURE_FIND_CONTEXT)|def(CONFIG_SELINUX)|def(CONFIG_FEATURE_SHOW_THREADS)|def(CONFIG_FEATURE_PS_WIDE))&(!def(CONFIG_FEATURE_PS_WIDE)|!def(CONFIG_DESKTOP)))"
