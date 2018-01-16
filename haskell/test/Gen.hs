module Gen where

import Test.QuickCheck
import Control.Monad (liftM, liftM3, liftM2)
import VProp

genDim :: Gen String
genDim = arbitrary :: Gen String

instance (Arbitrary d, Arbitrary a) => Arbitrary (VProp d a) where
  arbitrary = sized arbVProp

arbVProp :: (Arbitrary a, Arbitrary d) => Int -> Gen (VProp d a)
arbVProp 0 = liftM Obj arbitrary
arbVProp n = frequency [ (1, liftM Obj arbitrary)
                       , (4, liftM3 Chc arbitrary l l)
                       , (4, liftM Neg l)
                       , (4, liftM2 Or l l)
                       , (4, liftM2 And l l)
                       , (4, liftM2 Impl l l)
                       , (4, liftM2 BiImpl l l)
                       ]
  where l = arbVProp (n `div` 2)
