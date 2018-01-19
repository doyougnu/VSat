module Gen where

import Data.Hashable
import Test.QuickCheck
import GHC.Generics (Generic)
import Control.DeepSeq
import VProp

genDim :: Gen String
genDim = arbitrary :: Gen String

genAlphaNum :: Gen Char
genAlphaNum = elements ['a'..'z']

genAlphaNumStr :: Gen String
genAlphaNumStr = listOf genAlphaNum

newtype ANString = ANString {unwrapANString :: String}
  deriving (Generic)

instance NFData ANString

instance Show ANString where
  show = show . unwrapANString

instance Hashable ANString where
  hashWithSalt x s = hashWithSalt x (unwrapANString s)

instance Ord ANString where
  compare x y = compare (unwrapANString x) (unwrapANString y)

instance Eq ANString where
  x == y = (unwrapANString x) == (unwrapANString y)

instance Arbitrary ANString where
  arbitrary = ANString <$> genAlphaNumStr

genVProp :: IO (VProp ANString Integer)
genVProp = generate arbitrary
