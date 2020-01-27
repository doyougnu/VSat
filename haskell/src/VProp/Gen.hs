module VProp.Gen where

import qualified Control.Arrow         as A ((&&&))
import           Control.Monad         (liftM2, liftM3)
import           Prelude               hiding (EQ, GT, LT)
import           Test.Tasty.QuickCheck (Arbitrary(..), Gen, arbitrary,
                                        arbitrarySizedIntegral, elements,
                                        frequency, generate, listOf, oneof,
                                        resize, sized, suchThat, genericShrink)

import           VProp.Core            (maxShared, noDupRefs, onlyBools, numVars)
import           Data.Text             (Text, pack, toUpper, singleton)
import           VProp.Types

type VarProp = VProp Var Var Var
type ReadableExpr = VIExpr Var Var

instance Arbitrary Readable where arbitrary = genAlphaNumStr
instance Arbitrary Var where arbitrary = Var <$> genAlphaNumStr
instance Arbitrary NPrim where arbitrary = genPrim
instance Arbitrary RefN where arbitrary  = genRefN
instance Arbitrary N_N where arbitrary   = genN_N
instance Arbitrary NN_N where arbitrary  = genNN_N
instance Arbitrary B_B where arbitrary   = genB_B
instance Arbitrary BB_B where arbitrary  = genBB_B
instance Arbitrary NN_B where arbitrary  = genNN_B

instance Arbitrary ReadableExpr where
  arbitrary = sized $ arbVIExpr genSharedDim arbitrary (repeat 3)
  shrink = genericShrink

-- | arbritrary instance for the generator monad
instance Arbitrary VarProp where
  arbitrary = sized $ arbVProp genSharedDim arbitrary (repeat 3, repeat 3)
  shrink (ChcB _ l r) = [l, r]
  shrink (OpIB _ _ _) = []
  shrink (OpBB _ l r) = [l, r]
  shrink (OpB _ r)    = pure r
  shrink x            = pure x

instance Arbitrary (ReadableProp Var) where
  arbitrary = sized $ arbVProp genSharedDim arbitrary (repeat 3, repeat 3)
  shrink (ChcB _ l r) = [l, r]
  shrink (OpIB _ _ _) = []
  shrink (OpBB _ l r) = [l, r]
  shrink (OpB _ r)    = pure r
  shrink x            = pure x


instance Arbitrary (ReadableProp Text) where
  arbitrary = sized $ arbVProp genSharedDim' arbitrary (repeat 3, repeat 3)
  shrink (ChcB _ l r) = [l, r]
  shrink (OpIB _ _ _) = []
  shrink (OpBB _ l r) = [l, r]
  shrink (OpB _ r)    = pure r
  shrink x            = pure x

-- | Generate only alphabetical characters
genAlphaNum :: Gen Text
genAlphaNum = singleton <$> elements ['a'..'z']

-- | generate a list of only alphabetical characters and convert to Dim
genAlphaNumStr :: Gen Text
genAlphaNumStr = fmap mconcat $ flip suchThat (not . null) $ listOf genAlphaNum

genDim :: Gen (Dim Var)
genDim = Dim . Var . toUpper <$> genAlphaNumStr

genSharedDim :: Gen (Dim Var)
genSharedDim = (Dim . Var . toUpper . pack) <$>
               elements (zipWith (\a b -> [a,b]) ['a'..'d'] ['a'..'d'])

genSharedDim' :: Gen (Dim Text)
genSharedDim' = (Dim . toUpper . pack) <$>
                elements (zipWith (\a b -> [a,b]) ['a'..'d'] ['a'..'d'])

genSharedVar :: Gen Var
genSharedVar = Var . singleton <$> elements ['a'..'j']

genVar :: Gen Var
genVar = Var <$> genAlphaNumStr

genDouble :: Gen NPrim
genDouble = D <$> arbitrary

genInt :: Gen NPrim
genInt = I <$> arbitrarySizedIntegral

genPrim :: Gen NPrim
genPrim = oneof [genDouble, genInt]

genLit :: Gen (VProp d a b)
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
genBB_B = elements [Impl, BiImpl, XOr, And, Or]

genNN_B :: Gen NN_B
genNN_B = elements [LT, LTE, GT, GTE, EQ, NEQ]

-- | Generate an arbritrary prop where any variable name in the boolean language
-- _does not_ occur in the integer language
arbVProp :: (Arbitrary a, Ord a, Arbitrary d) =>
  Gen (Dim d) -> Gen a -> ([Int], [Int]) -> Int -> Gen (VProp d a a)
arbVProp gd gv fs n = flip suchThat noDupRefs $ arbVProp_ gd gv fs n

-- | Generate an Arbitrary VProp, given a generator and counter these
-- frequencies can change for different depths. The counter is merely for a
-- `sized` call
arbVProp_ :: (Arbitrary a, Arbitrary d) =>
  Gen (Dim d) -> Gen a -> ([Int], [Int]) -> Int -> Gen (VProp d a a)
arbVProp_ _  gv _     0 = RefB <$> gv
arbVProp_ gd gv fs@(bfreqs, ifreqs) n
  = frequency $ zip bfreqs [ LitB <$> arbitrary
                           , RefB <$> gv
                           , (liftM3 ChcB gd l l)
                           , liftM2 OpB genB_B l
                           , liftM3 OpBB genBB_B l l
                           , liftM3 OpIB genNN_B l' l'
                           ]
  where l = arbVProp_ gd gv fs (n `div` 2)
        l' = arbVIExpr gd gv ifreqs (n `div` 2)


arbVPropStrOnly :: Gen (Dim Var) ->
                   Gen Var ->
                   ([Int], [Int]) ->
                   Int ->
                   Gen VarProp
arbVPropStrOnly _   gv _                   0 = RefB <$> gv
arbVPropStrOnly gd  gv fs@(bfreqs, ifreqs) n
  = frequency $ zip bfreqs [ LitB <$> arbitrary
                           , RefB <$> gv
                           , (liftM3 ChcB gd l l)
                           , liftM2 OpB genB_B l
                           , liftM3 OpBB genBB_B l l
                           , liftM3 OpIB genNN_B l' l'
                           ]
  where l = arbVPropStrOnly gd gv fs (n `div` 2)
        l' = arbVIExprStrOnly gd gv ifreqs (n `div` 2)

arbVPropIntOnly_ :: Gen (Dim Var) ->
                    Gen Var ->
                    ([Int], [Int]) ->
                    Int ->
                    Gen VarProp
arbVPropIntOnly_ _  gv _     0 = RefB <$> gv
arbVPropIntOnly_ gd gv fs@(bfreqs, ifreqs) n
  = frequency $ zip bfreqs [ LitB <$> arbitrary
                           , RefB <$> gv
                           , (liftM3 ChcB gd l l)
                           , liftM2 OpB genB_B l
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
vPropNoShare :: [Int] -> Gen VarProp
vPropNoShare xs = sized g
  where g :: Int -> Gen VarProp
        g = arbVPropStrOnly genDim genVar $ (id A.&&& id) xs

vPropShare :: [Int] -> Gen VarProp
vPropShare xs = sized g
  where g :: Int -> Gen VarProp
        g = arbVProp genSharedDim genSharedVar $ (id A.&&& id) xs


vPropNoShareIntOnly :: [Int] -> Gen VarProp
vPropNoShareIntOnly xs = sized g
  where g :: Int -> Gen VarProp
        g = arbVProp genDim genVar $ (id A.&&& id) xs

vPropShareIntOnly :: [Int] -> Gen VarProp
vPropShareIntOnly xs = sized g
  where g :: Int -> Gen VarProp
        g = arbVProp genSharedDim genSharedVar $ (id A.&&& id) xs

-- | Generate a random prop according to its arbritrary type class instance,
-- this has a strong likelihood of sharing
-- | generate with $ x <- genVProp :: (IO (VProp Var Var Var))
genVProp :: IO VarProp
genVProp = generate arbitrary

genReadable :: Gen VarProp
genReadable = arbitrary

-- | run With $ x <- generate . genBoolProp $ vPropNoShare (repeat 30)
genBoolProp :: (Arbitrary a) => Gen (VProp d a a) -> Gen (VProp d a a)
genBoolProp = flip suchThat onlyBools

genVPropAtSize :: (Arbitrary a, Arbitrary b) =>
  Integer -> Gen (VProp d a b) -> Gen (VProp d a b)
genVPropAtSize n = flip suchThat $ (==n) . numVars

genVPropAtShare :: Int -> Gen VarProp -> Gen VarProp
genVPropAtShare n = flip suchThat $ (>=n) . maxShared

genVPropAtSizeIntOnly :: (Arbitrary a, Arbitrary b) =>
  Int -> Gen (VProp d a b) -> Gen (VProp d a b)
genVPropAtSizeIntOnly = resize

genVPropAtShareIntOnly :: Int -> Gen VarProp -> Gen VarProp
genVPropAtShareIntOnly n = flip suchThat $ (==n) . maxShared
