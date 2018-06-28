module VProp.Types ( Var(..)
                   , Dim(..)
                   , VConfig
                   , DimBool
                   , Config
                   -- , VProp(..)
                   , NPrim(..)
                   , Op2(..)
                   , Opn(..)) where


import           Data.Data           (Data, Typeable)
import           Test.QuickCheck     (Arbitrary)
import           Test.QuickCheck.Gen
import           GHC.Generics        (Generic)
import           Data.String         (IsString)
import           Control.DeepSeq     (NFData)
import           Data.SBV            (SBool)
import           Data.Map            (Map)
import           Prelude  hiding     (LT, GT, EQ)


-- | A feature is a named, boolean configuration option.
newtype Var = Var { varName :: String }
  deriving (Data,Eq,IsString,Ord,Typeable,Generic,NFData,Arbitrary)

newtype Dim = Dim { dimName :: String }
  deriving (Data,Eq,IsString,Ord,Show,Typeable,Generic,NFData,Arbitrary)

type VConfig a = a -> SBool
type DimBool = Dim -> SBool
type Config = Map Dim Bool

--
-- * Syntax
--

-- | Boolean expressions with choices
data VProp a
   = BLit Bool
   | RefB a
   | OpB  B_B  !(VProp a)
   | OpBB BB_B !(VProp a)  !(VProp a)
   | OpIB NN_B !(VIExpr a) !(VIExpr a)
   | Opn  Opn  ![(VProp a)]
   | ChcB Dim  !(VProp a)  !(VProp a)
  deriving (Eq,Generic,Typeable,Functor,Traversable,Foldable,Ord)

-- | Integer Expressions with Choices
data VIExpr a
  = ILit NPrim
  | RefI a
  | OpI  N_N  !(VIExpr a)
  | OpII NN_N !(VIExpr a) !(VIExpr a)
  | ChcI Dim  !(VIExpr a) !(VIExpr a)
  deriving (Eq,Generic,Typeable,Functor,Traversable,Foldable,Ord)

-- | data constructor for Numeric operations
data NPrim = I Int | F Float
  deriving (Eq,Generic,Typeable,Ord)

-- | Unary Numeric Operator
data N_N = Neg | Abs deriving (Eq,Generic,Data,Typeable,Show,Ord)

-- | Binary Boolean operators
data B_B = Not deriving (Eq,Generic,Data,Typeable,Show,Ord)

-- | Binary Numeric Operators
data NN_N = Add | Sub | Mult | Div deriving (Eq,Generic,Data,Typeable,Show,Ord)

-- | Binary Boolean operators
data BB_B = Impl | BiImpl deriving (Eq,Generic,Data,Typeable,Show,Ord)

-- | Binary Numeric predicate operators
data NN_B = LT | LTE | GT | GTE | EQ deriving (Eq,Generic,Data,Typeable,Show,Ord)

-- | N-ary logical operators
data Opn = And | Or deriving (Eq,Generic,Data,Typeable,Show,Ord)

-- | Unary Operators
data Op1 a = N_N N_N a
  deriving (Eq,Generic,Typeable,Functor,Traversable,Foldable,Ord)

-- | Binary Operators
data Op2 a = NN_B NN_B (Op2I a) (Op2I a)
  deriving (Eq,Generic,Typeable,Functor,Traversable,Foldable,Ord)

data Op2I a = NN_N NN_N a a
  deriving (Eq,Generic,Typeable,Functor,Traversable,Foldable,Ord)

x :: VProp String
x = Opn And [RefB "a", OpIB LT
                       (ChcI "A"
                         (OpI Neg (ILit $ I 5))
                         (OpII Add (ILit $ F 3.0) (RefI "c")))
                       (ChcI "C" (RefI "d") (RefI "a"))]
