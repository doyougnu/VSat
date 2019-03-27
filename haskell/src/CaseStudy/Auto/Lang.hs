module CaseStudy.Auto.Lang where

import           Utils (fromList,fromList')
import           Data.List (delete)
import           Data.SBV (EqSymbolic(..), literal)
import           Data.Bifunctor
import           Data.Bifoldable
import           Data.Bitraversable
import           Data.Monoid ((<>))
import           GHC.Generics
import           Control.DeepSeq (NFData)

import Debug.Trace
import qualified VProp.Types as V
import SAT (Boolean(..))

data AutoLang a b = AutoLit Bool
                  | AutoRef a
                  | Ctx RBOp (ALang b) (AutoLang a b)
                  | AutoNot (AutoLang a b)
                  | BBinary BOp (AutoLang a b) (AutoLang a b)
                  | RBinary RBOp (ALang b) (ALang b)
                deriving (Eq, Ord, Functor, Foldable, Traversable, Generic)

data BOp = And | Or | Impl | Eqv | Xor deriving (Eq, Ord,Generic)
data RBOp = LST | EQL | GRT | LSTE  | NEQL | GRTE  deriving (Eq,Ord,Generic)

data ALang a = ALit Integer
             | AVar a
             | ACtx (ALang a)
             | Neg (ALang a)
             | ABinary AOp (ALang a) (ALang a)
             deriving (Eq, Ord, Functor, Foldable, Traversable, Generic)

data AOp = Add | Subtract | Multiply | Divide | Modulus deriving (Eq, Ord,Generic)

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
instance (NFData a, NFData b) => NFData (AutoLang a b)
instance (NFData a) => NFData (ALang a )
instance NFData AOp
instance NFData RBOp
instance NFData BOp

atMost1 :: (Eq a,Eq b) => [AutoLang a b] -> AutoLang a b
atMost1 [] = error "empty list on input of atMost1"
atMost1 [x] = x
atMost1 xs = cs &&& fromList' (&&&) disjuncs
  where disjuncs = [(bnot x ||| bnot y) | (x, i) <- labeled
                                              , (y, j) <- labeled
                                              , x /= y
                                              , i < j
                                              ]

        labeled = zip xs [1..]
        cs = fromList (|||) xs

instance Boolean (AutoLang a b) where
  true  = AutoLit True
  false = AutoLit False
  bnot  = AutoNot
  (&&&) = BBinary And
  (|||) = BBinary Or
  (<+>) = BBinary Xor
  (==>) = BBinary Impl
  (<=>) = BBinary Eqv

instance V.Prim (AutoLang a b) (ALang b) where
  (.<)  = RBinary LST
  (.<=) = RBinary LSTE
  (.==) = RBinary EQL
  (./=) = RBinary NEQL
  (.>=) = RBinary GRTE
  (.>)  = RBinary GRT

instance V.PrimN (ALang a) where
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

instance (EqSymbolic a, EqSymbolic b) => EqSymbolic (AutoLang a b) where
  (AutoLit b)      .== (AutoLit b')        = b .== b'
  (AutoRef r)      .== (AutoRef r')        = r .== r'
  (Ctx op a p)     .== (Ctx op' a' p')     = (op .== op') &&& (a .== a') &&&
                                             (p .== p')
  (AutoNot e)      .== (AutoNot e')        = e .== e'
  (BBinary op l r) .== (BBinary op' l' r') = (op .== op') &&& (l .== l') &&&
                                             (r .== r')
  (RBinary op l r) .== (RBinary op' l' r') = (op .== op') &&&
                                             (l .== l') &&& (r .== r')
  _                .== _                   = false

instance EqSymbolic a => EqSymbolic (ALang a) where
  (ALit i) .== (ALit i') = (literal i) .== (literal i')
  (AVar a) .== (AVar b)  = a .== b
  (ACtx a) .== (ACtx b)  = a .== b
  (Neg a)  .== (Neg b)   = a .== b
  (ABinary op l r) .== (ABinary op' l' r') = (op .== op') &&&
                                             (l .== l') &&& (r .== r')
  _                .==  _ = false

instance EqSymbolic AOp where
  Add .== Add           = true
  Subtract .== Subtract = true
  Multiply .== Multiply = true
  Divide .== Divide     = true
  Modulus .== Modulus   = true
  _       .== _         = false

instance EqSymbolic RBOp where
  GRT .== GRT   = true
  GRTE .== GRTE = true
  EQL .== EQL   = true
  LST .== LST   = true
  LSTE .== LSTE = true
  NEQL .== NEQL = true
  _    .== _    = false

instance EqSymbolic BOp where
  And .== And   = true
  Or  .== Or    = true
  Impl .== Impl = true
  Eqv .== Eqv   = true
  Xor .== Xor   = true
  _   .== _     = false

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
