module VProp.Gen where

import           Control.Monad       (liftM2, liftM3)
import           Test.QuickCheck     (oneof, arbitrarySizedIntegral,  Arbitrary
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
import VProp.Core (maxShared)

-- | A wrapper to represent readable strings
newtype Readable = Re { readStr :: String }

instance Show Readable where
  show = show . readStr

instance Arbitrary Readable where
  arbitrary = Re <$> genAlphaNumStr

-- | arbritrary instance for the generator monad
instance Arbitrary a => Arbitrary (VProp a a) where
  arbitrary = sized $ arbVProp genSharedDim arbitrary (repeat 3, repeat 3)

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

genVar :: Gen Var
genVar = Var <$> genAlphaNumStr

genDouble :: Gen NPrim
genDouble = D <$> arbitrary

genInt :: Gen NPrim
genInt = I <$> arbitrarySizedIntegral

genPrim :: Gen NPrim
genPrim = oneof [genDouble, genInt]

genLit :: Gen (VProp a b)
genLit = LitB <$> arbitrary

frequencies :: Gen Int
frequencies = elements [1..10]

-- | Data constructor generators
genN_N :: Gen N_N
genN_N = elements [Neg, Abs, Sign]

genB_B :: Gen B_B
genB_B = elements [Not]

genNN_N :: Gen NN_N
genNN_N = elements [Add, Sub, Mult, Div, Mod]

genBB_B :: Gen BB_B
genBB_B = elements [Impl, BiImpl, XOr]

genNN_B :: Gen NN_B
genNN_B = elements [LT, LTE, GT, GTE, EQ, NEQ]

genOpn :: Gen Opn
genOpn = elements [And, Or]

-- | Generate an Arbitrary VProp, given a generator and counter these
-- frequencies can change for different depths. The counter is merely for a
-- `sized` call
arbVProp :: Arbitrary a =>
  Gen Dim -> Gen a -> ([Int], [Int]) -> Int -> Gen (VProp a a)
arbVProp _  gv _     0 = RefB <$> gv
arbVProp gd gv fs@(bfreqs, ifreqs) n
  = frequency $ zip bfreqs [ LitB <$> arbitrary
                           , RefB <$> gv
                           , (liftM3 ChcB gd l l)
                           , liftM2 OpB genB_B l
                           , (liftM2 (&&&) l l)
                           , (liftM2 (|||) l l)
                           , liftM3 OpBB genBB_B l l
                           , liftM3 OpIB genNN_B l' l'
                           ]
  where l = arbVProp gd gv fs (n `div` 2)
        l' = arbVIExpr gd gv ifreqs (n `div` 2)

arbVIExpr :: Arbitrary a =>
  Gen Dim -> Gen a -> [Int] -> Int -> Gen (VIExpr a)
arbVIExpr _ gv _ 0 = RefI <$> gv
arbVIExpr gd gv ifreqs n = frequency $ zip ifreqs [ LitI <$> genPrim
                                                  , RefI <$> gv
                                                  , liftM2 OpI genN_N l
                                                  , liftM3 OpII genNN_N l l
                                                  , liftM3 ChcI gd l l
                                                  ]
  where l = arbVIExpr gd gv ifreqs (n `div` 2)

-- | Generate a random prop term with no sharing among dimensions
vPropNoShare :: [Int] -> Gen (VProp Var Var)
vPropNoShare = sized . arbVProp genDim genVar . (id A.&&& id)

vPropShare :: [Int] -> Gen (VProp Var Var)
vPropShare = sized . arbVProp genSharedDim genSharedVar . (id A.&&& id)

-- | Generate a random prop according to its arbritrary type class instance,
-- this has a strong likelihood of sharing
-- | generate with $ x <- genVProp :: (IO (VProp Readable))
genVProp :: (Arbitrary a) => IO (VProp a a)
genVProp = generate arbitrary

-- vPropChoicesOverRefs = sized $ flip arbProp

genVPropAtSize :: (Arbitrary a, Arbitrary b) =>
  Int -> Gen (VProp a b) -> Gen (VProp a b)
genVPropAtSize = resize

genVPropAtShare :: (Arbitrary a, Arbitrary b) =>
  Int -> Gen (VProp a b) -> Gen (VProp a b)
genVPropAtShare n = flip suchThat $ (==n) . maxShared
