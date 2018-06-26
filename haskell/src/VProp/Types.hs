module VProp.Types ( Var(..)
                   , Dim(..)
                   , VConfig
                   , DimBool
                   , Config
                   , VProp(..)
                   , Prim(..)
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

-- | Boolean expressions over features.
data VProp a
   = Lit Prim
   | Ref !a
   | Not !(VProp a)
   | Op2 Op2 !(VProp a) !(VProp a)
   | Opn Opn ![(VProp a)]
   | Chc Dim !(VProp a) !(VProp a)
  deriving (Data,Eq,Generic,Typeable,Functor,Traversable,Foldable,Ord)

-- | data constructor for binary operations
data Prim = B Bool | I Int deriving (Eq,Generic,Data,Typeable,Show,Ord)
-- TODO is ORD appropriate here?
data Op2 = Impl | BiImpl | VLT | VLTE | VGT | VGTE | VEQ
  deriving (Eq,Generic,Data,Typeable,Show,Ord)
data Opn = And | Or deriving (Eq,Generic,Data,Typeable,Show,Ord)
