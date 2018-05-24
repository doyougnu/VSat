module VProp.Types where


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

type VConfig a b = a -> b
type DimBool = Dim -> SBool
type Config = Map Dim Bool

--
-- * Syntax
--

-- | Boolean expressions over features.
data VProp a
   = Lit Bool
   | Ref !a
   | Chc Dim !(VProp a) !(VProp a)
   | Not !(VProp a)
   | Opn Opn ![(VProp a)]
   | Op2 Op2 !(VProp a) !(VProp a)
  deriving (Data,Eq,Generic,Typeable,Functor,Traversable,Foldable)

-- | data constructor for binary operations
data Op2 = Impl | BiImpl deriving (Eq,Generic,Data,Typeable, Show)
data Opn = And | Or deriving (Eq,Generic,Data,Typeable, Show)
