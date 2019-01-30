module CaseStudy.Auto.Lang where

import           Utils (fromList)
import           Data.SBV (Boolean(..))

import           VProp.Types (Prim(..), PrimN(..))

data AutoLang a = AutoLit Bool
                | AutoRef a
                | Ctx RBOp (ALang a) (AutoLang a)
                | AutoNot (AutoLang a)
                | BBinary BOp (AutoLang a) (AutoLang a)
                | RBinary RBOp (ALang a) (ALang a)
                deriving (Eq, Ord, Functor, Foldable, Traversable)

data BOp = And | Or | Impl | Eqv | Xor deriving (Eq, Ord)
data RBOp = GRT | GRTE | EQL | LST | LSTE  | NEQL deriving (Eq, Ord)

data ALang a = ALit Integer
             | AVar a
             | ACtx (ALang a)
             | Neg (ALang a)
             | ABinary AOp (ALang a) (ALang a)
             deriving (Eq, Ord, Functor, Foldable, Traversable)

data AOp = Add | Subtract | Multiply | Divide | Modulus deriving (Eq, Ord)

prettyAuto :: (Show a) => AutoLang a -> String
prettyAuto = top
  where
    top :: (Show a) => AutoLang a -> String
    top (BBinary b l r)  = mconcat [sub l, " ", show b, " ", sub r]
    top (RBinary nb l r) = mconcat [prettyAuto' l, " ", show nb, " ", prettyAuto' r]
    top (AutoNot r)      = mconcat ["¬", prettyAuto r]
    top (Ctx rb al rs)   = mconcat ["Ctx", show rb," ", prettyAuto' al," ", prettyAuto rs]
    top e                = sub e

    sub :: (Show a) => AutoLang a -> String
    sub (AutoLit b) = if b then "#T" else "#F"
    sub (AutoRef a) = show a
    sub e           = "(" ++ top e ++ ")"

prettyAuto' :: Show a => ALang a -> String
prettyAuto' (ALit i) = show i
prettyAuto' (ACtx expr) = mconcat ["ACtx: ", show expr]
prettyAuto' (AVar a) = show a
prettyAuto' (Neg a)  = mconcat ["−", prettyAuto' a]
prettyAuto' (ABinary o l r) = mconcat [prettyAuto' l, " ", show o, " ", prettyAuto' r]


instance Show AOp where show Add      = "+"
                        show Subtract = "-"
                        show Multiply = "*"
                        show Divide   = "/"
                        show Modulus  = "%"

instance Show RBOp where show LST  = "<"
                         show LSTE = "≤"
                         show GRT  = ">"
                         -- show GRTE = "≥"
                         show GRTE = ">="
                         show EQL  = "=="
                         show NEQL = "≠"

instance Show BOp where show Impl = "→"
                        show Eqv  = "↔"
                        show Xor  = "⊻"
                        show And  = "∧"
                        show Or   = "∨"

instance Show a => Show (AutoLang a) where show = prettyAuto
instance Show a => Show (ALang a) where show = prettyAuto'

xAOrJoin :: [AutoLang a] -> AutoLang a
xAOrJoin = fromList $ BBinary Xor

instance Boolean (AutoLang a) where
  true  = AutoLit True
  false = AutoLit False
  bnot  = AutoNot
  (&&&) = BBinary And
  (|||) = BBinary Or
  (<+>) = BBinary Xor
  (==>) = BBinary Impl
  (<=>) = BBinary Eqv

instance Prim (AutoLang a) (ALang a) where
  (.<)  = RBinary LST
  (.<=) = RBinary LSTE
  (.==) = RBinary EQL
  (./=) = RBinary NEQL
  (.>=) = RBinary GRTE
  (.>)  = RBinary GRT

instance PrimN (ALang a) where
  (./) = ABinary Divide
  (.%) = ABinary Modulus

instance Num (ALang a) where
  fromInteger = ALit
  (+) = ABinary Add
  (*) = ABinary Multiply
  (-) = ABinary Subtract
  negate = Neg
  signum = error "signum not supported in AutoLang"
  abs    = error "absolute value not supported in AutoLang"
