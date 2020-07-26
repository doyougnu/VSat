module Proof where

open import Data.Bool
open import Data.Product using (_×_; proj₁; proj₂) renaming (_,_ to ⟨_,_⟩)
open import Data.String using (String)
open import Relation.Binary.PropositionalEquality using (_≡_; refl; sym; cong)
open import Data.Sum using (_⊎_; inj₁; inj₂)
-- Lists
open import Data.List using (List; []; _∷_; _++_; length; reverse; map; foldr; downFrom)
open import Data.List.Relation.Unary.Any using (Any; here; there)
open import Data.List.Membership.Propositional using (_∈_)

data Opb : Set where Not : Opb

data Vpl : Set where
  vLit  : Bool   → Vpl
  vRef  : String → Vpl
  vNeg  : Vpl    → Vpl
  vAnd  : Vpl → Vpl → Vpl
  vOr   : Vpl → Vpl → Vpl
  chc  : String → Vpl → Vpl → Vpl

data IL : Set where
  refIL : String → IL
  litIO : Bool → IL
  andIL : IL → IL → IL
  orIL  : IL → IL → IL
  chcIL : String → Vpl → Vpl → IL

data C₂ : Set where
  true  : C₂
  false : C₂
  cNeg   : C₂ → C₂
  cAnd   : C₂ → C₂ → C₂
  cOr    : C₂ → C₂ → C₂

dimensions : Vpl → List String
dimensions (chc d l r) = d ∷ dimensions l ++ dimensions r
dimensions (vAnd l r)  = dimensions l ++ dimensions r
dimensions (vOr  l r)  = dimensions l ++ dimensions r
dimensions _           = []

names : List (String × Bool) → List String
names = map proj₁

totalconfigure : ∀ {a : Vpl} {conf : List (String × Bool)} → names conf ≡ (dimensions a) → List (String × Bool) → Vpl → C₂
totalconfigure _ _ (vLit b) with b
...                         | true = true
...                         | false = false
totalconfigure conf (chc dim l r) with dim ∈ conf
...                          | true   =
...                          | false  =
