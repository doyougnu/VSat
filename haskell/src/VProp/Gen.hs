module VProp.Gen where

import           Control.Monad       (liftM2, liftM3)
import           Test.Tasty.QuickCheck ( oneof
                                       , arbitrarySizedIntegral
                                       , Arbitrary
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
import Prelude hiding (LT,EQ,GT)
import qualified Control.Arrow as A ((&&&))

import VProp.Types
import VProp.Core (maxShared, onlyBools, noDupRefs)

-- | A wrapper to represent readable strings
newtype Readable = Re { readStr :: String }
instance Show Readable where show = show . readStr

instance Arbitrary Readable where arbitrary = Re <$> genAlphaNumStr
instance Arbitrary Var where arbitrary = Var <$> genAlphaNumStr


-- | arbritrary instance for the generator monad
instance (Arbitrary a, Ord a, Arbitrary b, Ord b, Show a, Show b) =>
  Arbitrary (VProp Dim a b) where
  arbitrary = sized $
              arbVProp genSharedDim arbitrary arbitrary (repeat 3, repeat 3)

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
  zipWith  (\a b -> Dim $ toUpper <$> [a, b]) ['a'..'d'] ['a'..'d']

genSharedVar :: Gen Var
genSharedVar = elements $ Var . show <$> ['a'..'j']

genSharedIVar :: Gen Var
genSharedIVar = elements $ Var . show <$> ['k'..]

genVar :: Gen Var
genVar = Var <$> genAlphaNumStr

genDouble :: Gen NPrim
genDouble = D <$> arbitrary

genInt :: Gen NPrim
genInt = I <$> arbitrarySizedIntegral

genPrim :: Gen NPrim
genPrim = oneof [genDouble, genInt]

genLit :: Gen (VProp a b c)
genLit = LitB <$> arbitrary

frequencies :: Gen Int
frequencies = elements [1..10]

-- | Data constructor generators
genN_N :: Gen N_N
genN_N = elements [Neg, Abs, Sign]

genRefN :: Gen RefN
genRefN = elements [RefI, RefD]

genB_B :: Gen B_B
genB_B = elements [Not]

-- MOD will fail if there is no integer in the expression. Will be a stack
-- overflow or memory error
genNN_N :: Gen NN_N
genNN_N = elements [Add, Sub, Mult, Div]

genBB_B :: Gen BB_B
genBB_B = elements [Impl, BiImpl, XOr]

genNN_B :: Gen NN_B
genNN_B = elements [LT, LTE, GT, GTE, EQ, NEQ]

genOpn :: Gen Opn
genOpn = elements [And, Or]

-- | Generate an arbritrary prop where any variable name in the boolean language
-- _does not_ occur in the integer language
arbVProp :: (Arbitrary a, Ord a, Arbitrary b, Ord b, Show a, Show b) =>
  Gen Dim -> Gen a -> Gen b -> ([Int], [Int]) -> Int -> Gen (VProp Dim a b)
arbVProp gd gv giv fs n = flip suchThat noDupRefs $ arbVProp_ gd gv giv fs n

-- | Generate an Arbitrary VProp, given a generator and counter these
-- frequencies can change for different depths. The counter is merely for a
-- `sized` call
arbVProp_ :: (Arbitrary a, Arbitrary b) =>
  Gen Dim -> Gen a -> Gen b -> ([Int], [Int]) -> Int -> Gen (VProp Dim a b)
arbVProp_ _  gv _    _     0 = RefB <$> gv
arbVProp_ gd gv giv fs@(bfreqs, ifreqs) n
  = frequency $ zip bfreqs [ LitB <$> arbitrary
                           , RefB <$> gv
                           , (liftM3 ChcB gd l l)
                           , liftM2 OpB genB_B l
                           , (liftM2 (&&&) l l)
                           , (liftM2 (|||) l l)
                           , liftM3 OpBB genBB_B l l
                           , liftM3 OpIB genNN_B l' l'
                           ]
  where l  = arbVProp_ gd gv giv fs (n `div` 2)
        l' = arbVIExpr gd giv ifreqs (n `div` 2)

arbVIExpr :: Arbitrary a =>
  Gen Dim -> Gen a -> [Int] -> Int -> Gen (VIExpr Dim a)
arbVIExpr _ gv _ 0 = liftM2 Ref genRefN gv
arbVIExpr gd gv ifreqs n = frequency $ zip ifreqs [ LitI <$> genPrim
                                                  , liftM2 OpI genN_N l
                                                  , liftM3 OpII genNN_N l l
                                                  , liftM3 ChcI gd l l
                                                  ]
  where l = arbVIExpr gd gv ifreqs (n `div` 2)

-- | Generate a random prop term with no sharing among dimensions
vPropNoShare :: [Int] -> Gen (VProp Dim Var Var)
vPropNoShare = sized . arbVProp genDim genVar genVar . (id A.&&& id)

vPropShare :: [Int] -> Gen (VProp Dim Var Var)
vPropShare = sized . arbVProp genSharedDim genSharedVar genSharedIVar . (id A.&&& id)

-- | Generate a random prop according to its arbritrary type class instance,
-- this has a strong likelihood of sharing
-- | generate with $ x <- genVProp :: (IO (VProp Var Var))
genVProp :: (Arbitrary a, Arbitrary b, Ord a, Ord b, Show a, Show b) =>
  IO (VProp Dim a b)
genVProp = generate arbitrary

-- | run With $ x <- generate . genBoolProp $ vPropNoShare (repeat 30)
genBoolProp :: (Arbitrary a) => Gen (VProp a a b) -> Gen (VProp a a b)
genBoolProp = flip suchThat onlyBools

genVPropAtSize :: (Arbitrary a, Arbitrary b, Arbitrary c) =>
  Int -> Gen (VProp a b c) -> Gen (VProp a b c)
genVPropAtSize = resize

genVPropAtShare :: (Arbitrary a, Arbitrary b, Arbitrary c, Show a) =>
  Int -> Gen (VProp a b c) -> Gen (VProp a b c)
genVPropAtShare n = flip suchThat $ (==n) . maxShared
