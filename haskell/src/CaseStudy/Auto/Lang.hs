{-# LANGUAGE DeriveAnyClass #-}
module CaseStudy.Auto.Lang where

import           Utils (fromList)
import           Data.SBV (Boolean(..), EqSymbolic)
import           Data.Bifunctor
import           Data.Bifoldable
import           Data.Bitraversable
import           Data.Monoid ((<>))

import           VProp.Types (Prim(..), PrimN(..))

data AutoLang a b = AutoLit Bool
                  | AutoRef a
                  | Ctx RBOp (ALang b) (AutoLang a b)
                  | AutoNot (AutoLang a b)
                  | BBinary BOp (AutoLang a b) (AutoLang a b)
                  | RBinary RBOp (ALang b) (ALang b)
                deriving (Eq, Ord, Functor, Foldable, Traversable, EqSymbolic)

data BOp = And | Or | Impl | Eqv | Xor deriving (Eq, Ord)
data RBOp = GRT | GRTE | EQL | LST | LSTE  | NEQL deriving (Eq, Ord)

data ALang a = ALit Integer
             | AVar a
             | ACtx (ALang a)
             | Neg (ALang a)
             | ABinary AOp (ALang a) (ALang a)
             deriving (Eq, Ord, Functor, Foldable, Traversable, EqSymbolic)

data AOp = Add | Subtract | Multiply | Divide | Modulus deriving (Eq, Ord)

prettyAuto :: (Show a, Show b) => AutoLang a b -> String
prettyAuto = top
  where
    top :: (Show a, Show b) => AutoLang a  b-> String
    top (BBinary b l r)  = mconcat [sub l, " ", show b, " ", sub r]
    top (RBinary nb l r) = mconcat [prettyAuto' l, " ", show nb, " ", prettyAuto' r]
    top (AutoNot r)      = mconcat ["¬", prettyAuto r]
    top (Ctx rb al rs)   = mconcat ["Ctx", show rb," ", prettyAuto' al," ", prettyAuto rs]
    top e                = sub e

    sub :: (Show a, Show b) => AutoLang a b -> String
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

instance (Show a, Show b) => Show (AutoLang a b) where show = prettyAuto
instance Show a => Show (ALang a) where show = prettyAuto'

xAOrJoin :: [AutoLang a b] -> AutoLang a b
xAOrJoin = fromList $ BBinary Xor

instance Boolean (AutoLang a b) where
  true  = AutoLit True
  false = AutoLit False
  bnot  = AutoNot
  (&&&) = BBinary And
  (|||) = BBinary Or
  (<+>) = BBinary Xor
  (==>) = BBinary Impl
  (<=>) = BBinary Eqv

instance Prim (AutoLang a b) (ALang b) where
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

instance Bifunctor AutoLang where
  bimap _ _ (AutoLit b) = AutoLit b
  bimap f _ (AutoRef a) = AutoRef $ f a
  bimap f g (AutoNot e) = AutoNot $ bimap f g e
  bimap f g (BBinary op l r) = BBinary op (bimap f g l) (bimap f g r)
  bimap _ g (RBinary op l r) = RBinary op (g <$> l) (g <$> r)
  bimap f g (Ctx op aexpr rest) = Ctx op (g <$> aexpr) $ bimap f g rest

instance Bifoldable AutoLang where
  bifoldMap _ _ (AutoLit _) = mempty
  bifoldMap f _ (AutoRef a) = f a
  bifoldMap f g (AutoNot e) = bifoldMap f g e
  bifoldMap f g (BBinary _ l r) = (bifoldMap f g l) <> (bifoldMap f g r)
  bifoldMap _ g (RBinary _ l r) = (foldMap g l) <> (foldMap g r)
  bifoldMap f g (Ctx _ aexpr rest) = (foldMap g aexpr) <> bifoldMap f g rest

instance Bitraversable AutoLang where
  bitraverse _ _ (AutoLit e) = pure $ AutoLit e
  bitraverse f _ (AutoRef a) = AutoRef <$> f a
  bitraverse f g (AutoNot e) = AutoNot <$> bitraverse f g e
  bitraverse f g (BBinary op l r) =
    BBinary op <$> bitraverse f g l <*> bitraverse f g r
  bitraverse _ g (RBinary op l r) = RBinary op <$> traverse g l <*> traverse g r
  bitraverse f g (Ctx op aexpr rest) =
    Ctx op <$> traverse g aexpr <*> bitraverse f g rest
