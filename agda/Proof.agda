module Proof where

open import Data.Bool
open import Data.Product using (_×_; proj₁; proj₂) renaming (_,_ to ⟨_,_⟩)
open import Data.String using (String; _==_)
open import Relation.Binary.PropositionalEquality as Eq
open Eq using (_≡_; refl; sym; cong)
open Eq.≡-Reasoning
open import Data.Sum using (_⊎_; inj₁; inj₂)
open import Data.Vec as V using (Vec; _∷_; []; _++_;map;fromList)
open import Data.Vec.Membership.Setoid using (_∈_)
open import Data.Vec.Relation.Unary.All using (All)
open import Data.Nat
open import Data.Maybe using (Maybe; just; nothing)
open import Data.List as L using (List; _∷_; []; _++_; replicate)

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
  cLit   : Bool → C₂
  cNeg   : C₂ → C₂
  cAnd   : C₂ → C₂ → C₂
  cOr    : C₂ → C₂ → C₂

-- | Count the non-unique dimensions
dimCount : Vpl → ℕ
dimCount (chc d l r) = suc (dimCount l + dimCount r)
dimCount (vAnd l r)  = dimCount l + dimCount r
dimCount (vOr l r)   = dimCount l + dimCount r
dimCount (vNeg e)    = dimCount e
dimCount _           = zero

-- | A projection from a vpl formula to a vector of dimensions
dimensions : ∀ (v : Vpl) → Vec String (dimCount v)
dimensions (chc d l r) = d V.∷ dimensions l V.++ dimensions r
dimensions (vAnd  l r) = dimensions l V.++ dimensions r
dimensions (vOr   l r) = dimensions l V.++ dimensions r
dimensions (vNeg  s)   = dimensions s
dimensions (vLit _)    = V.[]
dimensions (vRef _)    = V.[]

names : ∀ {n} → Vec (String × Bool) n → Vec String n
names = map proj₁

-- Seems like the standard agda way to lookup an element is to use a setoid,
-- Any, and losing a witness. I'm not committed to it yet
get : {A : Set} {n : ℕ} → (A → Bool) → Vec A n → Maybe A
get _ [] = nothing
get p (x ∷ xs) with p x
...            | true = just x
...            | false = get p xs

-- | Configuration of a vpl formula, if ∃ dimension ∈ (f : Vpl) s.t. dimension ∉
-- | Configuration then the choice is preserved
configure : ∀ {n} → Vec (String × Bool) n → Vpl → Vpl
-- computation rule
configure conf (chc dim l r) with get (λ x → (proj₁ x) == dim) conf
...                          | nothing        = (chc dim (configure conf l) (configure conf r))
...                          | just selection = if proj₂ selection
                                                then configure conf l
                                                else configure conf r
-- congruence rules
configure c (vAnd l r) = vAnd (configure c l) (configure c r)
configure c (vOr l r)  = vOr (configure c l) (configure c r)
configure _ x = x

-- | An embedding relation. An embedding is a weakened isomorphism. So Set A
-- | embeds B if there is a morphism from A → B, B → A, s.t. from ∘ to but not
-- | to ∘ from
infix 0 _≲_
record _≲_ (A B : Set) : Set where
  field
    to      : A → B
    from    : B → A
    from∘to : ∀ (x : A) → from (to x) ≡ x
open _≲_

≲-refl : ∀ {A : Set} → A ≲ A
≲-refl =
  record
    { to      = λ{x → x}
    ; from    = λ{y → y}
    ; from∘to = λ{x → refl}
    }


≲-trans : ∀ {A B C : Set} → A ≲ B → B ≲ C → A ≲ C
≲-trans A≲B B≲C =
  record
    { to      = λ{x → to   B≲C (to   A≲B x)}
    ; from    = λ{y → from A≲B (from B≲C y)}
    ; from∘to = λ{x →
        begin
          from A≲B (from B≲C (to B≲C (to A≲B x)))
        ≡⟨ cong (from A≲B) (from∘to B≲C (to A≲B x)) ⟩
          from A≲B (to A≲B x)
        ≡⟨ from∘to A≲B x ⟩
          x
        ∎}
     }

data Total {n : ℕ} : Vpl → Vec (String × Bool) n → Set where
  istotal : ∀ {v : Vpl} {n : ℕ} {conf : Vec (String × Bool) n}
    → All (_∈ (names conf)) (dimensions v)
    ------------------------------
    → Total v conf
