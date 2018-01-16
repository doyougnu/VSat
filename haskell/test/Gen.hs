module Gen where

import Test.QuickCheck
import Control.Monad (liftM, liftM3, liftM2)
import VProp

genDim :: Gen String
genDim = arbitrary :: Gen String

genAlphaNum :: Gen Char
genAlphaNum = elements ['a'..'z']

genAlphaNumStr :: Gen String
genAlphaNumStr = listOf genAlphaNum

newtype ANString = ANString {unwrapASString :: String}

instance Show ANString where
  show = show . unwrapASString

instance Arbitrary ANString where
  arbitrary = ANString <$> genAlphaNumStr
