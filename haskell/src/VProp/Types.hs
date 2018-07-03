module VProp.Types ( module Prelude
                   , Var(..)
                   , Dim(..)
                   , VConfig
                   , DimBool
                   , Config
                   , VProp(..)
                   , VIExpr(..)
                   , B_B(..), BB_B(..)
                   , N_N(..), NN_N(..)
                   , NN_B(..)
                   , NPrim(..)
                   , Opn(..)
                   , (.<)
                   , (.<=)
                   , (.==)
                   , (./=)
                   , (.>=)
                   , (.>)
                   , S.true
                   , S.false
                   , S.bnot
                   , (S.&&&)
                   , (S.|||)
                   , (S.<+>)
                   , (S.==>)
                   , (S.<=>)
                   , (./)
                   , (.%)
                   , iRef
                   , ref) where

import           Data.Data           (Data, Typeable)
import           Data.Fixed          (mod')
import           Test.QuickCheck     (Arbitrary)
import           Test.QuickCheck.Gen
import           GHC.Generics        (Generic)
import           Data.String         (IsString)
import           Control.DeepSeq     (NFData)
import qualified Data.SBV as S
import           Data.SBV.Internals (liftQRem, liftDMod)
import           Data.Map            (Map)
import           Prelude  hiding     (LT, GT, EQ, lookup)


-- | A feature is a named, boolean configuration option.
newtype Var = Var { varName :: String }
  deriving (Data,Eq,IsString,Ord,Typeable,Generic,NFData,Arbitrary)

newtype Dim = Dim { dimName :: String }
  deriving (Data,Eq,IsString,Ord,Show,Typeable,Generic,NFData,Arbitrary)

type VConfig a b = a -> b
type DimBool = Dim -> S.SBool
type Config = Map Dim Bool

--
-- * Syntax
--

-- | This Design taken from Eric Walkingshaw with great respect :)

-- | Boolean expressions with choices
data VProp a
   = LitB Bool
   | RefB a
   | OpB  B_B  !(VProp a)
   | OpBB BB_B !(VProp a)  !(VProp a)
   | OpIB NN_B !(VIExpr a) !(VIExpr a)
   | Opn  Opn  ![(VProp a)]
   | ChcB Dim  !(VProp a)  !(VProp a)
  deriving (Eq,Generic,Typeable,Functor,Traversable,Foldable,Ord)

-- | Integer Expressions with Choices
data VIExpr a
  = LitI NPrim
  | RefI a
  | OpI  N_N  !(VIExpr a)
  | OpII NN_N !(VIExpr a) !(VIExpr a)
  | ChcI Dim  !(VIExpr a) !(VIExpr a)
  deriving (Eq,Generic,Typeable,Functor,Traversable,Foldable,Ord)

-- | data constructor for Numeric operations
data NPrim = I Integer | D Double
  deriving (Eq,Generic,Typeable,Ord)

-- | Unary Numeric Operator
data N_N = Neg | Abs | Sign deriving (Eq,Generic,Data,Typeable,Ord)

-- | Binary Boolean operators
data B_B = Not deriving (Eq,Generic,Data,Typeable,Ord)

-- | Binary Numeric Operators
data NN_N = Add | Sub | Mult | Div | Mod deriving (Eq,Generic,Data,Typeable,Ord)

-- | Binary Boolean operators
data BB_B = Impl | BiImpl | XOr deriving (Eq,Generic,Data,Typeable,Ord)

-- | Binary Numeric predicate operators
data NN_B = LT | LTE | GT | GTE | EQ | NEQ deriving (Eq,Generic,Data,Typeable,Ord)

-- | N-ary logical operators
data Opn = And | Or deriving (Eq,Generic,Data,Typeable,Show,Ord)

-- | add div and mod to num
class Num n => PrimN n where
  (./), (.%) :: n -> n -> n

-- | Overload the primitive operators
class (S.Boolean b, PrimN n) => Prim b n where
  (.<), (.<=), (.==), (./=), (.>=), (.>) :: n -> n -> b

infix 4 .<, .<=, .==, ./=, .>=, .>
infixl 7 ./, .%

-- | some not so smart constructors, pinning a to string because we will be
-- using String the most
iRef :: String -> VIExpr String
iRef = RefI

ref :: a -> VProp a
ref = RefB

-- | Begin primitive instances

instance PrimN Integer where
  (./) = div
  (.%) = mod

instance PrimN Double where
  (./) = (/)
  (.%) = mod'

instance Prim Bool Integer where
  (.<)  = (<)
  (.<=) = (<=)
  (.==) = (==)
  (./=) = (/=)
  (.>=) = (>=)
  (.>)  = (>)

instance Prim Bool Double where
  (.<)  = (<)
  (.<=) = (<=)
  (.==) = (==)
  (./=) = (/=)
  (.>=) = (>=)
  (.>)  = (>)

-- | SBV instances

instance PrimN S.SInteger where
  (./)  = S.sDiv
  (.%)  = S.sMod

instance S.SDivisible Double where
  sQuotRem x 0.0 = (0.0, x)
  sQuotRem x y = x `S.sQuotRem` y
  sDivMod  x 0.0 = (0.0, x)
  sDivMod  x y = x `S.sDivMod` y

instance S.SDivisible S.SDouble where
  sQuotRem = liftQRem
  sDivMod  = liftDMod

instance PrimN S.SDouble where
  (./)  = S.sDiv
  (.%)  = S.sMod

instance PrimN S.SInt8 where
  (./)  = S.sDiv
  (.%)  = S.sMod

instance PrimN S.SInt16 where
  (./)  = S.sDiv
  (.%)  = S.sMod

instance PrimN S.SInt32 where
  (./)  = S.sDiv
  (.%)  = S.sMod

instance PrimN S.SInt64 where
  (./)  = S.sDiv
  (.%)  = S.sMod

instance Prim S.SBool S.SInteger where
  (.<)  = (S..<)
  (.<=) = (S..<=)
  (.==) = (S..==)
  (./=) = (S../=)
  (.>=) = (S..>=)
  (.>)  = (S..>)

instance Prim S.SBool S.SInt8 where
  (.<)  = (S..<)
  (.<=) = (S..<=)
  (.==) = (S..==)
  (./=) = (S../=)
  (.>=) = (S..>=)
  (.>)  = (S..>)

instance Prim S.SBool S.SInt16 where
  (.<)  = (S..<)
  (.<=) = (S..<=)
  (.==) = (S..==)
  (./=) = (S../=)
  (.>=) = (S..>=)
  (.>)  = (S..>)

instance Prim S.SBool S.SInt32 where
  (.<)  = (S..<)
  (.<=) = (S..<=)
  (.==) = (S..==)
  (./=) = (S../=)
  (.>=) = (S..>=)
  (.>)  = (S..>)

instance Prim S.SBool S.SInt64 where
  (.<)  = (S..<)
  (.<=) = (S..<=)
  (.==) = (S..==)
  (./=) = (S../=)
  (.>=) = (S..>=)
  (.>)  = (S..>)

instance Prim S.SBool S.SDouble where
  (.<)  = (S..<)
  (.<=) = (S..<=)
  (.==) = (S..==)
  (./=) = (S../=)
  (.>=) = (S..>=)
  (.>)  = (S..>)

-- | We can treat a variational proposition as a boolean formulae
instance S.Boolean (VProp a) where
  true  = LitB True
  false = LitB True
  bnot  = OpB Not
  l &&& r = Opn And [l,r]
  l ||| r = Opn Or  [l,r]
  (<+>) = OpBB XOr
  (==>) = OpBB Impl
  (<=>) = OpBB BiImpl

-- | Boilerplate to make Num (VIExpr a) work out
instance Num (NPrim) where
  fromInteger = I . fromInteger
  abs = abs
  negate = negate
  signum = signum
  (+) = (+)
  (-) = (-)
  (*) = (*)

-- | We can treat Variational integer expressions like nums
instance Num (VIExpr a) where
  fromInteger = LitI . fromInteger
  abs    = OpI Abs
  negate = OpI Neg
  signum = OpI Sign
  (+)    = OpII Add
  (-)    = OpII Sub
  (*)    = OpII Mult

-- | the other num instances
instance PrimN (VIExpr a) where
  (./) = OpII Div
  (.%) = OpII Mod

instance Prim (VProp a) (VIExpr a) where
  (.<)  = OpIB LT
  (.<=) = OpIB LTE
  (.==) = OpIB EQ
  (./=) = OpIB NEQ
  (.>=) = OpIB GTE
  (.>)  = OpIB GT
