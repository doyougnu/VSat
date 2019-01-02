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
instance Arbitrary Var where
  arbitrary = Var <$> genAlphaNumStr


-- | arbritrary instance for the generator monad
instance Arbitrary (VProp Var Var) where
  arbitrary = sized $ arbVProp genSharedDim arbitrary (repeat 3, repeat 3)

-- | Generate only alphabetical characters
genAlphaNum :: Gen Char
genAlphaNum = elements ['a'..'z']

-- | generate a list of only alphabetical characters and convert to Dim
genAlphaNumStr :: Gen String
genAlphaNumStr = flip suchThat (not . null) $ listOf genAlphaNum

genDim :: Gen (Dim Var)
genDim = Dim . Var <$> (fmap . fmap) toUpper genAlphaNumStr

genSharedDim :: Gen (Dim Var)
genSharedDim = elements $
  zipWith  (\a b -> Dim . Var $ toUpper <$> [a, b]) ['a'..'d'] ['a'..'d']

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
arbVProp :: (Arbitrary a, Ord a) =>
  Gen (Dim a) -> Gen a -> ([Int], [Int]) -> Int -> Gen (VProp a a)
arbVProp gd gv fs n = flip suchThat noDupRefs $ arbVProp_ gd gv fs n

-- | Generate an Arbitrary VProp, given a generator and counter these
-- frequencies can change for different depths. The counter is merely for a
-- `sized` call
arbVProp_ :: Arbitrary a =>
  Gen (Dim a) -> Gen a -> ([Int], [Int]) -> Int -> Gen (VProp a a)
arbVProp_ _  gv _     0 = RefB <$> gv
arbVProp_ gd gv fs@(bfreqs, ifreqs) n
  = frequency $ zip bfreqs [ LitB <$> arbitrary
                           , RefB <$> gv
                           , (liftM3 ChcB gd l l)
                           , liftM2 OpB genB_B l
                           , (liftM2 (&&&) l l)
                           , (liftM2 (|||) l l)
                           , liftM3 OpBB genBB_B l l
                           , liftM3 OpIB genNN_B l' l'
                           ]
  where l = arbVProp_ gd gv fs (n `div` 2)
        l' = arbVIExpr gd gv ifreqs (n `div` 2)


arbVPropStrOnly :: Gen (Dim Var) ->
                   Gen Var ->
                   ([Int], [Int]) ->
                   Int ->
                   Gen (VProp Var Var)
arbVPropStrOnly _   gv _                   0 = RefB <$> gv
arbVPropStrOnly gd  gv fs@(bfreqs, ifreqs) n
  = frequency $ zip bfreqs [ LitB <$> arbitrary
                           , RefB <$> gv
                           , (liftM3 ChcB gd l l)
                           , liftM2 OpB genB_B l
                           , (liftM2 (&&&) l l)
                           , (liftM2 (|||) l l)
                           , liftM3 OpBB genBB_B l l
                           , liftM3 OpIB genNN_B l' l'
                           ]
  where l = arbVPropStrOnly gd gv fs (n `div` 2)
        l' = arbVIExprStrOnly gd gv ifreqs (n `div` 2)

arbVPropIntOnly_ :: Gen (Dim Var) ->
                    Gen Var ->
                    ([Int], [Int]) ->
                    Int ->
                    Gen (VProp Var Var)
arbVPropIntOnly_ _  gv _     0 = RefB <$> gv
arbVPropIntOnly_ gd gv fs@(bfreqs, ifreqs) n
  = frequency $ zip bfreqs [ LitB <$> arbitrary
                           , RefB <$> gv
                           , (liftM3 ChcB gd l l)
                           , liftM2 OpB genB_B l
                           , (liftM2 (&&&) l l)
                           , (liftM2 (|||) l l)
                           , liftM3 OpBB genBB_B l l
                           , liftM3 OpIB genNN_B l' l'
                           ]
  where l = arbVProp_ gd gv fs (n `div` 2)
        l' = arbVIExprIntOnly gd gv ifreqs (n `div` 2)


arbVIExpr :: (Arbitrary a, Arbitrary b) =>
             Gen (Dim a) ->
             Gen b ->
             [Int] ->
             Int ->
             Gen (VIExpr a b)
arbVIExpr _ gv _ 0 = liftM2 Ref genRefN gv
arbVIExpr gd gv ifreqs n = frequency $ zip ifreqs [ LitI <$> genPrim
                                                  , liftM2 OpI genN_N l
                                                  , liftM3 OpII genNN_N l l
                                                  , liftM3 ChcI gd l l
                                                  ]
  where l = arbVIExpr gd gv ifreqs (n `div` 2)

arbVIExprStrOnly :: Gen (Dim Var) ->
             Gen Var ->
             [Int] ->
             Int ->
             Gen (VIExpr Var Var)
arbVIExprStrOnly _ gv _ 0 = liftM2 Ref genRefN gv
arbVIExprStrOnly gd gv ifreqs n = frequency $ zip ifreqs [ LitI <$> genPrim
                                                  , liftM2 OpI genN_N l
                                                  , liftM3 OpII genNN_N l l
                                                  , liftM3 ChcI gd l l
                                                  ]
  where l = arbVIExpr gd gv ifreqs (n `div` 2)


arbVIExprIntOnly :: (Arbitrary a, Arbitrary b) =>
  Gen (Dim a) -> Gen b -> [Int] -> Int -> Gen (VIExpr a b)
arbVIExprIntOnly _ gv _ 0 = liftM2 Ref genRefN gv
arbVIExprIntOnly gd gv ifreqs n = frequency $ zip ifreqs [ LitI <$> genInt
                                                  , liftM2 OpI genN_N l
                                                  , liftM3 OpII genNN_N l l
                                                  , liftM3 ChcI gd l l
                                                  ]
  where l = arbVIExpr gd gv ifreqs (n `div` 2)

-- | Generate a random prop term with no sharing among dimensions
vPropNoShare :: [Int] -> Gen (VProp Var Var)
vPropNoShare xs = sized g
  where g :: Int -> Gen (VProp Var Var)
        g = arbVPropStrOnly genDim genVar $ (id A.&&& id) xs

vPropShare :: [Int] -> Gen (VProp Var Var)
vPropShare xs = sized g
  where g :: Int -> Gen (VProp Var Var)
        g = arbVProp genSharedDim genSharedVar $ (id A.&&& id) xs


vPropNoShareIntOnly :: [Int] -> Gen (VProp Var Var)
vPropNoShareIntOnly xs = sized g
  where g :: Int -> Gen (VProp Var Var)
        g = arbVProp genDim genVar $ (id A.&&& id) xs

vPropShareIntOnly :: [Int] -> Gen (VProp Var Var)
vPropShareIntOnly xs = sized g
  where g :: Int -> Gen (VProp Var Var)
        g = arbVProp genSharedDim genSharedVar $ (id A.&&& id) xs

-- | Generate a random prop according to its arbritrary type class instance,
-- this has a strong likelihood of sharing
-- | generate with $ x <- genVProp :: (IO (VProp Var Var))
genVProp :: IO (VProp Var Var)
genVProp = generate arbitrary

-- | run With $ x <- generate . genBoolProp $ vPropNoShare (repeat 30)
genBoolProp :: (Arbitrary a) => Gen (VProp a a) -> Gen (VProp a a)
genBoolProp = flip suchThat onlyBools

genVPropAtSize :: (Arbitrary a, Arbitrary b) =>
  Int -> Gen (VProp a b) -> Gen (VProp a b)
genVPropAtSize = resize

genVPropAtShare :: Int -> Gen (VProp Var Var) -> Gen (VProp Var Var)
genVPropAtShare n = flip suchThat $ (==n) . maxShared

genVPropAtSizeIntOnly :: (Arbitrary a, Arbitrary b) =>
  Int -> Gen (VProp a b) -> Gen (VProp a b)
genVPropAtSizeIntOnly = resize

genVPropAtShareIntOnly :: Int -> Gen (VProp Var Var) -> Gen (VProp Var Var)
genVPropAtShareIntOnly n = flip suchThat $ (==n) . maxShared
