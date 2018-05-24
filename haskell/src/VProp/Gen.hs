module VProp.Gen where

import           Control.Monad       (liftM2, liftM3)
import           Test.QuickCheck     ( Arbitrary
                                     , Gen
                                     , arbitrary
                                     , suchThat
                                     , elements
                                     , listOf
                                     , frequency
                                     , sized
                                     , generate
                                     , resize)
import           Data.Char           (toUpper)

import VProp.Types
import VProp.SBV

-- | A wrapper to represent readable strings
newtype Readable = Re { readStr :: String }

instance Show Readable where
  show = show . readStr

instance Arbitrary Readable where
  arbitrary = Re <$> genAlphaNumStr

-- | arbritrary instance for the generator monad
instance Arbitrary a => Arbitrary (VProp a) where
  arbitrary = sized $ arbVProp genSharedDim arbitrary (repeat 3)

-- | Generate only alphabetical characters
genAlphaNum :: Gen Char
genAlphaNum = elements ['a'..'z']

-- | generate a list of only alphabetical characters and convert to Dim
genAlphaNumStr :: Gen String
genAlphaNumStr = flip suchThat (not . null) $ listOf genAlphaNum

genDim :: Gen Dim
genDim = Dim <$> (fmap . fmap) toUpper genAlphaNumStr

genSharedDim :: Gen Dim
genSharedDim = elements $
  zipWith  (\a b -> Dim $ toUpper <$> [a, b]) ['a'..'f'] ['a'..'f']

genSharedVar :: Gen Var
genSharedVar = elements $ Var . show <$> ['a'..'j']

genVar :: Gen Var
genVar = Var <$> genAlphaNumStr

frequencies :: Gen Int
frequencies = elements [1..10]

-- | Generate an Arbitrary VProp, given a generator and counter these
-- frequencies can change for different depths. The counter is merely for a
-- `sized` call
arbVProp :: Arbitrary a => Gen Dim -> Gen a -> [Int] -> Int -> Gen (VProp a)
arbVProp _  gv _     0 = Ref <$> gv
arbVProp gd gv freqs n = frequency $ zip freqs [ (fmap Ref gv)
                                               , (liftM3 Chc gd l l)
                                               , (fmap Not l)
                                               , (liftM2 (&&&) l l)
                                               , (liftM2 (|||) l l)
                                               , (liftM2 (==>) l l)
                                               , (liftM2 (<=>) l l)
                                               ]
  where l = arbVProp gd gv freqs (n `div` 2)

-- | Generate a random prop term with no sharing among dimensions
vPropNoShare :: [Int] -> Gen (VProp Var)
vPropNoShare = sized . arbVProp genDim genVar

vPropShare :: [Int] -> Gen (VProp Var)
vPropShare = sized . arbVProp genSharedDim genSharedVar

-- | Generate a random prop according to its arbritrary type class instance,
-- this has a strong likelihood of sharing
-- | generate with $ x <- genVProp :: (IO (VProp String))
genVProp :: Arbitrary a => IO (VProp a)
genVProp = generate arbitrary

-- vPropChoicesOverRefs = sized $ flip arbProp

genVPropAtSize :: Arbitrary a => Int -> Gen (VProp a) -> Gen (VProp a)
genVPropAtSize = resize