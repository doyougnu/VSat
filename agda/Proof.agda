module Proof where

open import Data.Bool
open import Data.String

data Opb : Set where Not : Opb

data Opbb : Set where
  or   : Opbb
  and  : Opbb
  impl : Opbb
  eqv  : Opbb

data Vpl : Set where
  lit  : Bool   → Vpl
  ref  : String → Vpl
  opB  : Opb    → Vpl → Vpl
  opBB : Opbb   → Vpl → Vpl
