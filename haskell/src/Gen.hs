module Gen (genVProp) where

import Data.Hashable
import Test.QuickCheck.Gen
import Test.QuickCheck.Arbitrary
import GHC.Generics (Generic)
import Control.DeepSeq
import VProp

genAlphaNum :: Gen Char
genAlphaNum = elements ['a'..'z']

genAlphaNumStr :: Gen String
genAlphaNumStr = listOf genAlphaNum

newtype ANString = ANString {unwrapANString :: String}
  deriving (Generic)

newtype BInt = BInt {unwrapBInt :: Integer}
  deriving (Generic, Integral, Ord, Num, Eq, Enum, Real)

instance NFData ANString
instance NFData BInt

instance Bounded BInt where
  minBound = 1
  maxBound = 5

instance Arbitrary BInt where
  arbitrary = arbitrarySizedBoundedIntegral

instance Show BInt where
  show = show . unwrapBInt

instance Show ANString where
  show = show . unwrapANString

instance Hashable ANString where
  hashWithSalt x s = hashWithSalt x (unwrapANString s)

instance Ord ANString where
  compare x y = compare (unwrapANString x) (unwrapANString y)

instance Eq ANString where
  x == y = unwrapANString x == unwrapANString y

instance Arbitrary ANString where
  arbitrary = ANString <$> genAlphaNumStr

genVProp :: IO (VProp ANString BInt)
genVProp = generate arbitrary
