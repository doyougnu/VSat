module VProp.Types ( Var(..)
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
                   , PrimN(..)
                   , Prim(..)
                   , S.true
                   , S.false
                   , S.bnot
                   , (S.&&&)
                   , (S.|||)
                   , (S.<+>)
                   , (S.==>)
                   , (S.<=>)
                   , bifoldMap
                   , bimap
                   , bifoldr
                   , bitraverse
                   , iRef
                   , iLit
                   , dRef
                   , dLit
                   , bRef
                   , iChc
                   , bChc) where

import           Control.DeepSeq       (NFData)
import           Data.Bifoldable       (Bifoldable, bifoldMap, bifoldr)
import           Data.Bifunctor        (Bifunctor, bimap)
import           Data.Bitraversable    (Bitraversable, bitraverse)
import           Data.Data             (Data, Typeable)
import           Data.Fixed            (mod')
import           Data.Map              (Map)
import           Data.Monoid           ((<>))
import qualified Data.SBV              as S
import qualified Data.Sequence         as SE
import           Data.String           (IsString)
import           GHC.Generics          (Generic)
import           Prelude               hiding (EQ, GT, LT, lookup)
import           Test.Tasty.QuickCheck


-- | A feature is a named, boolean configuration option.
newtype Var = Var { varName ::  String}
  deriving (Data,Eq,IsString,Ord,Typeable,Generic,NFData)

newtype Dim a = Dim { dimName ::  a}
  deriving (Data,Eq,IsString,Ord,Show,Typeable,Generic,NFData,Arbitrary,Functor,Traversable,Foldable)

type VConfig a b = a -> b
type DimBool a = (Dim a) -> S.SBool
type Config  a = Map (Dim a) Bool

--
-- * Syntax
--

-- | This Design taken from Eric Walkingshaw with great respect :)

-- | Boolean expressions with choices
data VProp a b
   = LitB Bool
   | RefB b
   | OpB  B_B  !(VProp a b)
   | OpBB BB_B !(VProp a b) !(VProp a b)
   | OpIB NN_B !(VIExpr a b)  !(VIExpr a b)
   | Opn  Opn  !(SE.Seq (VProp a b))
   | ChcB (Dim a)  !(VProp a b) !(VProp a b)
  deriving (Eq,Generic,Typeable,Functor,Traversable,Foldable,Ord)

-- | Integer Expressions with Choices
data VIExpr a b
  = LitI NPrim
  | Ref RefN b
  | OpI  N_N  !(VIExpr a b)
  | OpII NN_N !(VIExpr a b) !(VIExpr a b)
  | ChcI (Dim a)  !(VIExpr a b) !(VIExpr a b)
  deriving (Eq,Generic,Typeable,Functor,Traversable,Foldable,Ord)

-- | Mirroring NPrim with Symbolic types for the solver
data SNum = SI S.SInt64
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
class Num n => PrimN n where
  (./), (.%) :: n -> n -> n

-- | Overload the primitive operators
class (S.Boolean b, PrimN n) => Prim b n where
  (.<), (.<=), (.==), (./=), (.>=), (.>) :: n -> n -> b

infix 4 .<, .<=, .==, ./=, .>=, .>
infixl 7 ./, .%

-- | some not so smart constructors, pinning a to string because we will be
-- using String the most
iRef :: a -> VIExpr d a
iRef = Ref RefI

iLit :: Integer -> VIExpr a b
iLit = LitI . I

dLit :: Double -> VIExpr a b
dLit = LitI . D

dRef :: a -> VIExpr d a
dRef = Ref RefD

bRef :: b -> VProp d b
bRef = RefB

bChc :: d -> VProp d b -> VProp d b -> VProp d b
bChc x = ChcB (Dim x)

iChc :: d -> VIExpr d a -> VIExpr d a -> VIExpr d a
iChc x = ChcI (Dim x)

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

instance Prim (VProp a b) Integer where
  (.<)  i j = OpIB LT  (LitI $ I i) (LitI $ I j)
  (.<=) i j = OpIB LTE (LitI $ I i) (LitI $ I j)
  (.==) i j = OpIB EQ  (LitI $ I i) (LitI $ I j)
  (./=) i j = OpIB NEQ (LitI $ I i) (LitI $ I j)
  (.>=) i j = OpIB GTE (LitI $ I i) (LitI $ I j)
  (.>)  i j = OpIB GT  (LitI $ I i) (LitI $ I j)

instance Prim (VProp a b) Double where
  (.<)  i j = OpIB LT  (LitI $ D i) (LitI $ D j)
  (.<=) i j = OpIB LTE (LitI $ D i) (LitI $ D j)
  (.==) i j = OpIB EQ  (LitI $ D i) (LitI $ D j)
  (./=) i j = OpIB NEQ (LitI $ D i) (LitI $ D j)
  (.>=) i j = OpIB GTE (LitI $ D i) (LitI $ D j)
  (.>)  i j = OpIB GT  (LitI $ D i) (LitI $ D j)

-- * SBV instances

-- | we'll need to mirror the NPrim data type in SBV via SNum
instance Num SNum where
  fromInteger = SI . S.literal . fromInteger

  abs (SI i) = SI $ abs i
  abs (SD d) = SD $ S.fpAbs d

  negate (SI i) = SI $ negate i
  negate (SD d) = SD $ S.fpNeg d

  signum (SI i) = SI $ signum i
  signum (SD d) = SD $ signum (S.fromSDouble S.sRoundNearestTiesToAway d)

  (SI i) + (SI i') = SI $ i + i'
  (SD d) + (SI i)  = SD $ d + (S.sFromIntegral i)
  (SI i) + (SD d)  = SD $ (S.sFromIntegral i) + d
  (SD d) + (SD d') = SD $ d + d'

  (SI i) - (SI i') = SI $ i - i'
  (SD d) - (SI i)  = SD $ d - S.sFromIntegral i
  (SI i) - (SD d)  = SD $ S.sFromIntegral i - d
  (SD d) - (SD d') = SD $ d - d'

  (SI i) * (SI i') = SI $ i * i'
  (SD d) * (SI i)  = SD $ d * S.sFromIntegral i
  (SI i) * (SD d)  = SD $ d * S.sFromIntegral i
  (SD d) * (SD d') = SD $ d * d'

instance PrimN SNum where
  (SI i) ./ (SI i') = SI $ i ./ i'
  (SD d) ./ (SI i)  = SD $ d ./ (S.sFromIntegral i)
  (SI i) ./ (SD d)  = SD $ (S.sFromIntegral i) ./ d
  (SD d) ./ (SD d') = SD $ d ./ d'


  (SI i) .% (SI i') = SI $ i .% i'
  (SD d) .% (SI i)  = SI $ (S.fromSDouble S.sRoundNearestTiesToAway d) .% i
  (SI i) .% (SD d)  = SI $ i .% (S.fromSDouble S.sRoundNearestTiesToAway d)
  (SD d) .% (SD d') = SD $ S.fpRem d d'

instance PrimN S.SDouble where
  (./)  = S.fpDiv S.sRoundNearestTiesToAway
  (.%)  = undefined

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

instance S.Mergeable SNum where
  symbolicMerge _ b thn els
    | Just result <- S.unliteral b = if result then thn else els
    | otherwise = els

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

instance Prim S.SBool SNum where
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

  (.==) (SI i) (SI i') = (S..==) i i'
  (.==) (SD d) (SI i') = (S..==) d (S.sFromIntegral i')
  (.==) (SI i) (SD d)  = (S..==) (S.sFromIntegral i) d
  (.==) (SD d) (SD d') = (S..==) d d'

  (./=) (SI i) (SI i') = (S../=) i i'
  (./=) (SD d) (SI i') = (S../=) d (S.sFromIntegral i')
  (./=) (SI i) (SD d)  = (S../=) (S.sFromIntegral i) d
  (./=) (SD d) (SD d') = (S../=) d d'

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
  l &&& r = Opn And $ l SE.<| (SE.singleton r)
  l ||| r = Opn Or  $ l SE.<| (SE.singleton r)
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
instance Num (VIExpr a b) where
  fromInteger = LitI . fromInteger
  abs    = OpI Abs
  negate = OpI Neg
  signum = OpI Sign
  (+)    = OpII Add
  (-)    = OpII Sub
  (*)    = OpII Mult

-- | the other num instances
instance PrimN (VIExpr a b) where
  (./) = OpII Div
  (.%) = OpII Mod

instance Prim (VProp a b) (VIExpr a b) where
  (.<)  = OpIB LT
  (.<=) = OpIB LTE
  (.==) = OpIB EQ
  (./=) = OpIB NEQ
  (.>=) = OpIB GTE
  (.>)  = OpIB GT


-- * structural instances
instance Bifunctor VIExpr where
  bimap _ g (Ref t b) = Ref t $ g b
  bimap f g (OpI op e) = OpI op (bimap f g e)
  bimap f g (OpII op l r) = OpII op (bimap f g l) (bimap f g r)
  bimap f g (ChcI d l r) = ChcI (f <$> d) (bimap f g l) (bimap f g r)
  bimap _ _ (LitI i) = LitI i


instance Bifunctor VProp where
  bimap _ g (RefB v)      = RefB $ g v
  bimap f g (OpB op e)    = OpB op (bimap f g e)
  bimap f g (OpBB op l r) = OpBB op (bimap f g l) (bimap f g r)
  bimap f g (OpIB op l r) = OpIB op (bimap f g l) (bimap f g r)
  bimap f g (ChcB d l r)  = ChcB (f <$> d) (bimap f g l) (bimap f g r)
  bimap f g (Opn op l)    = Opn op $ fmap (bimap f g) l
  bimap _ _ (LitB b)      = LitB b


instance Bifoldable VIExpr where
  bifoldMap _ g (Ref _ a)     = g a
  bifoldMap f g (OpI _ e)     = bifoldMap f g e
  bifoldMap f g (OpII _ l r)  = bifoldMap f g l <> bifoldMap f g r
  bifoldMap f g (ChcI d l r)  = (f $ dimName d) <> bifoldMap f g l <> bifoldMap f g r
  bifoldMap _ _ (LitI _)      = mempty


instance Bifoldable VProp where
  bifoldMap _ g (RefB a)     = g a
  bifoldMap f g (OpB _ e)    = bifoldMap f g e
  bifoldMap f g (OpBB _ l r) = bifoldMap f g l <> bifoldMap f g r
  bifoldMap f g (OpIB _ l r) = bifoldMap f g l <> bifoldMap f g r
  bifoldMap f g (ChcB d l r) = (f $ dimName d) <>
                               bifoldMap f g l <> bifoldMap f g r
  bifoldMap f g (Opn _ l)    = foldMap (bifoldMap f g) l
  bifoldMap _ _ (LitB _)     = mempty


instance Bitraversable VIExpr where
  bitraverse _ g (Ref op v)    = Ref op <$> g v
  bitraverse f g (OpI op e)    = OpI op <$> bitraverse f g e
  bitraverse f g (OpII op l r) = OpII op <$> bitraverse f g l <*> bitraverse f g r
  bitraverse f g (ChcI d l r)  = ChcI <$> traverse f d <*>
                                 bitraverse f g l <*> bitraverse f g r
  bitraverse _ _ (LitI x)      = pure $ LitI x


instance Bitraversable VProp where
  bitraverse _ g (RefB v)      = RefB    <$> g v
  bitraverse f g (OpB op e)    = OpB  op <$> bitraverse f g e
  bitraverse f g (OpBB op l r) = OpBB op <$> bitraverse f g l <*> bitraverse f g r
  bitraverse f g (OpIB op l r) = OpIB op <$> bitraverse f g l <*> bitraverse f g r
  bitraverse f g (Opn op ls)   = Opn  op <$> traverse (bitraverse f g) ls
  bitraverse f g (ChcB d l r)  = ChcB <$>
                                 traverse f d <*>
                                 bitraverse f g l <*>
                                 bitraverse f g r
  bitraverse _ _ (LitB x)      = pure $ LitB x
