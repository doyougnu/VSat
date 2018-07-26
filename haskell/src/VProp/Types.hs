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
                   , SNum(..)
                   , RefN(..)
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
                   , bifoldMap
                   , bimap
                   , bifoldr
                   , bitraverse
                   , iRef
                   , dRef
                   , bRef) where

import           Data.Data           (Data, Typeable)
import           Data.Monoid         ((<>))
import           Data.Fixed          (mod')
import           Test.Tasty.QuickCheck
import           GHC.Generics        (Generic)
import           Data.String         (IsString)
import           Control.DeepSeq     (NFData)
import qualified Data.SBV as S
import           Data.SBV.Internals (liftQRem, liftDMod)
import           Data.Map            (Map)
import           Data.Bifunctor      (Bifunctor, bimap)
import           Data.Bitraversable  (Bitraversable, bitraverse)
import           Data.Bifoldable     (Bifoldable, bifoldMap, bifoldr)
import           Prelude  hiding     (LT, GT, EQ, lookup)


-- | A feature is a named, boolean configuration option.
newtype Var = Var { varName :: String }
  deriving (Data,Eq,IsString,Ord,Typeable,Generic,NFData)

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
data VProp a b
   = LitB Bool
   | RefB a
   | OpB  B_B  !(VProp a b)
   | OpBB BB_B !(VProp a b) !(VProp a b)
   | OpIB NN_B !(VIExpr b)  !(VIExpr b)
   | Opn  Opn  ![(VProp a b)]
   | ChcB Dim  !(VProp a b) !(VProp a b)
  deriving (Eq,Generic,Typeable,Functor,Traversable,Foldable,Ord)

-- | Integer Expressions with Choices
data VIExpr a
  = LitI NPrim
  | Ref RefN a
  | OpI  N_N  !(VIExpr a)
  | OpII NN_N !(VIExpr a) !(VIExpr a)
  | ChcI Dim  !(VIExpr a) !(VIExpr a)
  deriving (Eq,Generic,Typeable,Functor,Traversable,Foldable,Ord)

-- | Mirroring NPrim with Symbolic types for the solver
data SNum = SI S.SInteger
          | SD S.SDouble
          deriving (Eq, Show)

-- | data constructor for Numeric operations
data NPrim = I Integer | D Double
  deriving (Eq,Generic,Typeable,Ord)

-- | Reference types
data RefN = RefI | RefD deriving (Eq,Generic,Typeable,Ord)

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
class (Num n, Num m) => PrimN n m where
  (./), (.%) :: n -> m -> m

-- | Overload the primitive operators
class (S.Boolean b, PrimN n m) => Prim b n m where
  (.<), (.<=), (.==), (./=), (.>=), (.>) :: n -> m -> b

infix 4 .<, .<=, .==, ./=, .>=, .>
infixl 7 ./, .%

-- | some not so smart constructors, pinning a to string because we will be
-- using String the most
iRef :: String -> VIExpr String
iRef = Ref RefI

dRef :: String -> VIExpr String
dRef = Ref RefD

bRef :: String -> VProp String b
bRef = RefB

-- | Begin primitive instances

instance PrimN Integer Integer where
  (./) = div
  (.%) = mod

instance PrimN Double Double where
  (./) = (/)
  (.%) = mod'

instance Prim Bool Integer Integer where
  (.<)  = (<)
  (.<=) = (<=)
  (.==) = (==)
  (./=) = (/=)
  (.>=) = (>=)
  (.>)  = (>)

instance Prim Bool Double Double where
  (.<)  = (<)
  (.<=) = (<=)
  (.==) = (==)
  (./=) = (/=)
  (.>=) = (>=)
  (.>)  = (>)

instance Prim (VProp a b) Integer Integer where
  (.<)  i j = OpIB LT  (LitI $ I i) (LitI $ I j)
  (.<=) i j = OpIB LTE (LitI $ I i) (LitI $ I j)
  (.==) i j = OpIB EQ  (LitI $ I i) (LitI $ I j)
  (./=) i j = OpIB NEQ (LitI $ I i) (LitI $ I j)
  (.>=) i j = OpIB GTE (LitI $ I i) (LitI $ I j)
  (.>)  i j = OpIB GT  (LitI $ I i) (LitI $ I j)

instance Prim (VProp a b) Double Double where
  (.<)  i j = OpIB LT  (LitI $ D i) (LitI $ D j)
  (.<=) i j = OpIB LTE (LitI $ D i) (LitI $ D j)
  (.==) i j = OpIB EQ  (LitI $ D i) (LitI $ D j)
  (./=) i j = OpIB NEQ (LitI $ D i) (LitI $ D j)
  (.>=) i j = OpIB GTE (LitI $ D i) (LitI $ D j)
  (.>)  i j = OpIB GT  (LitI $ D i) (LitI $ D j)

-- * SBV instances

-- | we'll need to mirror the NPrim data type in SBV via SNum
instance Num SNum where
  fromInteger = SI . S.literal

  abs (SI i) = SI $ abs i
  abs (SD d) = SD $ abs d

  negate (SI i) = SI $ negate i
  negate (SD d) = SD $ negate d

  signum (SI i) = SI $ signum i
  signum (SD d) = SD $ signum d

  (SI i) + (SI i') = SI $ i + i'
  (SD d) + (SI i)  = SD $ d + S.sFromIntegral i
  (SI i) + (SD d)  = SD $ d + S.sFromIntegral i
  (SD d) + (SD d') = SD $ d + d'

  (SI i) - (SI i') = SI $ i - i'
  (SD d) - (SI i)  = SD $ d - S.sFromIntegral i
  (SI i) - (SD d)  = SD $ S.sFromIntegral i - d
  (SD d) - (SD d') = SD $ d - d'

  (SI i) * (SI i') = SI $ i * i'
  (SD d) * (SI i)  = SD $ d * S.sFromIntegral i
  (SI i) * (SD d)  = SD $ d * S.sFromIntegral i
  (SD d) * (SD d') = SD $ d * d'

instance PrimN SNum SNum where
  (SI i) ./ (SI i') = SI $ i ./ i'
  (SD d) ./ (SI i)  = SD $ d ./ (S.sFromIntegral i)
  (SI i) ./ (SD d)  = SD $ S.sFromIntegral i ./ d
  (SD d) ./ (SD d') = SD $ d ./ d'

  (SI i) .% (SI i') = SI $ i .% i'
  (SD d) .% (SI i)  = SI $ (S.sRealToSInteger $ (S.toRational d)) .% i
  (SI i) .% (SD d)  = SI $ i .% (S.sRealToSInteger d)
  (SD d) .% (SD d') = SI $ (S.sRealToSInteger d) .% (S.sRealToSInteger d')

instance PrimN NPrim SNum  where
  (I i) ./ (SI i')  = SI $ (S.literal i)                   ./ i'
  (I i) ./ (SD d')  = SD $ (S.sFromIntegral $ S.literal i) ./ d'
  (D d) ./ (SI i')  = SI $ (S.literal d)                   ./ (S.sFromIntegral i')
  (D d) ./ (SD d')  = SD $ (S.literal d)                   ./ d'

  (I i) .% (SI i')  = SI $ (S.literal i)                   .% i'
  (I i) .% (SD d')  = SD $ (S.sFromIntegral $ S.literal i) .% d'
  (D d) .% (SI i')  = SI $ (S.literal d)                   .% (S.sFromIntegral i')
  (D d) .% (SD d')  = SD $ (S.literal d)                   .% d'

instance PrimN S.SInteger S.SInteger where
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

instance PrimN S.SDouble S.SDouble where
  (./)  = S.sDiv
  (.%)  = S.sMod

instance PrimN S.SInt8 S.SInt8 where
  (./)  = S.sDiv
  (.%)  = S.sMod

instance PrimN S.SInt16 S.SInt16 where
  (./)  = S.sDiv
  (.%)  = S.sMod

instance PrimN S.SInt32 S.SInt32 where
  (./)  = S.sDiv
  (.%)  = S.sMod

instance PrimN S.SInt64 S.SInt64 where
  (./)  = S.sDiv
  (.%)  = S.sMod

instance S.Mergeable SNum where
  symbolicMerge _ b thn els
    | Just result <- S.unliteral b = if result then thn else els
  symbolicMerge _ _ _ _ = undefined -- quite -WALL

instance S.EqSymbolic SNum where
  (.==) (SI i) (SI i') = (S..==) i i'
  (.==) (SD d) (SI i') = (S..==) d (S.sFromIntegral i')
  (.==) (SI i) (SD d)  = (S..==) (S.sFromIntegral i) d
  (.==) (SD d) (SD d') = (S..==) d d'

  (./=) (SI i) (SI i') = (S../=) i i'
  (./=) (SD d) (SI i') = (S../=) d (S.sFromIntegral i')
  (./=) (SI i) (SD d)  = (S../=) (S.sFromIntegral i) d
  (./=) (SD d) (SD d') = (S../=) d d'

instance S.OrdSymbolic SNum where
  (.<) (SI i) (SI i') = (S..<) i i'
  (.<) (SD d) (SI i)  = (S..<) d (S.sFromIntegral i)
  (.<) (SI i) (SD d)  = (S..<) (S.sFromIntegral i) d
  (.<) (SD d) (SD d') = (S..<) d d'

  (.<=) (SI i) (SI i') = (S..<=) i i'
  (.<=) (SD d) (SI i)  = (S..<=) d (S.sFromIntegral i)
  (.<=) (SI i) (SD d)  = (S..<=) (S.sFromIntegral i) d
  (.<=) (SD d) (SD d') = (S..<=) d d'

  (.>=) (SI i) (SI i') = (S..>=) i i'
  (.>=) (SD d) (SI i)  = (S..>=) d (S.sFromIntegral i)
  (.>=) (SI i) (SD d)  = (S..>=) (S.sFromIntegral i) d
  (.>=) (SD d) (SD d') = (S..>=) d d'

  (.>) (SI i) (SI i') = (S..>) i i'
  (.>) (SD d) (SI i)  = (S..>) d (S.sFromIntegral i)
  (.>) (SI i) (SD d)  = (S..>) (S.sFromIntegral i) d
  (.>) (SD d) (SD d') = (S..>) d d'

instance Prim S.SBool SNum SNum where
  (.<)  = (S..<)
  (.<=) = (S..<=)
  (.==) = (S..==)
  (./=) = (S../=)
  (.>=) = (S..>=)
  (.>)  = (S..>)

instance Prim S.SBool S.SInteger S.SInteger where
  (.<)  = (S..<)
  (.<=) = (S..<=)
  (.==) = (S..==)
  (./=) = (S../=)
  (.>=) = (S..>=)
  (.>)  = (S..>)

instance Prim S.SBool S.SInt8 S.SInt8 where
  (.<)  = (S..<)
  (.<=) = (S..<=)
  (.==) = (S..==)
  (./=) = (S../=)
  (.>=) = (S..>=)
  (.>)  = (S..>)

instance Prim S.SBool S.SInt16 S.SInt16 where
  (.<)  = (S..<)
  (.<=) = (S..<=)
  (.==) = (S..==)
  (./=) = (S../=)
  (.>=) = (S..>=)
  (.>)  = (S..>)

instance Prim S.SBool S.SInt32 S.SInt32 where
  (.<)  = (S..<)
  (.<=) = (S..<=)
  (.==) = (S..==)
  (./=) = (S../=)
  (.>=) = (S..>=)
  (.>)  = (S..>)

instance Prim S.SBool S.SInt64 S.SInt64 where
  (.<)  = (S..<)
  (.<=) = (S..<=)
  (.==) = (S..==)
  (./=) = (S../=)
  (.>=) = (S..>=)
  (.>)  = (S..>)

instance Prim S.SBool S.SDouble S.SDouble where
  (.<)  = (S..<)
  (.<=) = (S..<=)
  (.==) = (S..==)
  (./=) = (S../=)
  (.>=) = (S..>=)
  (.>)  = (S..>)

-- | make prop mergeable so choices can use symbolic conditionals
instance S.Mergeable (VProp a b) where
  symbolicMerge _ b thn els
    | Just result <- S.unliteral b = if result then thn else els
  symbolicMerge _ _ _ _ = undefined -- quite -WALL

-- | We can treat a variational proposition as a boolean formulae
instance S.Boolean (VProp a b) where
  true  = LitB True
  false = LitB False
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
instance PrimN (VIExpr a) (VIExpr a) where
  (./) = OpII Div
  (.%) = OpII Mod

instance Prim (VProp a b) (VIExpr b) (VIExpr b) where
  (.<)  = OpIB LT
  (.<=) = OpIB LTE
  (.==) = OpIB EQ
  (./=) = OpIB NEQ
  (.>=) = OpIB GTE
  (.>)  = OpIB GT


-- * structural instances
instance Bifunctor VProp where
  bimap f _ (RefB v)      = RefB $ f v
  bimap f g (OpB op e)    = OpB op (bimap f g e)
  bimap f g (OpBB op l r) = OpBB op (bimap f g l) (bimap f g r)
  bimap _ g (OpIB op l r) = OpIB op (g <$> l) (g <$> r)
  bimap f g (ChcB d l r)  = ChcB d (bimap f g l) (bimap f g r)
  bimap f g (Opn op l)    = Opn op $ fmap (bimap f g) l
  bimap _ _ (LitB b)      = LitB b

instance Bifoldable VProp where
  bifoldMap f _ (RefB a)     = f a
  bifoldMap f g (OpB _ e)    = bifoldMap f g e
  bifoldMap f g (OpBB _ l r) = bifoldMap f g l <> bifoldMap f g r
  bifoldMap _ g (OpIB _ l r) = foldMap g l <> foldMap g r
  bifoldMap f g (ChcB _ l r) = bifoldMap f g l <> bifoldMap f g r
  bifoldMap f g (Opn _ l)    = foldMap (bifoldMap f g) l
  bifoldMap _ _ (LitB _)     = mempty


instance Bitraversable VProp where
  bitraverse f _ (RefB v) = RefB <$> f v
  bitraverse f g (OpB op e) = OpB op <$> bitraverse f g e
  bitraverse f g (OpBB op l r) = OpBB op <$> bitraverse f g l <*> bitraverse f g r
  bitraverse _ g (OpIB op l r) = OpIB op <$> traverse g l <*> traverse g r
  bitraverse f g (Opn op ls) = Opn op <$> traverse (bitraverse f g) ls
  bitraverse f g (ChcB d l r) = ChcB d <$> bitraverse f g l <*> bitraverse f g r
  bitraverse _ _ (LitB x)    = pure $ LitB x
