{-# OPTIONS --no-import-sorts #-}
module Lambda-full where

open import Agda.Primitive
  renaming (Set to Type; Setω to Typeω)
open import Agda.Builtin.Equality

variable
  A B C : Type

------------------------------------------------------------------------------
-- Identity type

Id : (A : Type) → (x y : A) → Type
Id A x y = x ≡ y

------------------------------------------------------------------------------
-- The empty type

data Empty : Type where

⊥-elim : {A : Type} → Empty → A
⊥-elim ()
------------------------------------------------------------------------------
-- Negation

¬_ : (A : Type) → Type
¬ A = A → Empty

------------------------------------------------------------------------------
-- The unit type

open import Agda.Builtin.Unit

------------------------------------------------------------------------------
-- Natural numbers
data ℕ : Type where
  zero
    ---
    : ℕ

  suc
    : (n : ℕ)
    → ----
      ℕ

-- Try to comment the following line
{-# BUILTIN NATURAL ℕ #-}

_ : 1 ≡ suc zero
_ = {!!}

------------------------------------------------------------------------------
-- List over A

data List (A : Type) : Type where
  []
    --------
    : List A

  _∷_
    : (x : A) (xs : List A)
    → ---------------------
      List A

infixr 5 _∷_

------------------------------------------------------------------------------
-- The memberhsip relation

variable
  x y   : A
  xs ys : List A

data _∋_ {A : Type} : List A → A → Type where
  zero
      ---------------------
    : (x ∷ xs) ∋ x
  suc
    : xs ∋ x
    → -----------------------
      (y ∷ xs) ∋ x

infix 4 _∋_

_ : 3 ∷ 1 ∷ 2 ∷ [] ∋ 1
_ = {! !}

_ : 3 ∷ 1 ∷ 2 ∷ [] ∋ 3
_ = {! !}

------------------------------------------------------------------------------
-- Types for STLC
    
-- Ty is for the *type* in the object language (STLC),
-- not the type in our meta-language (Agda).

data Ty (V : Type) : Type where
  `_
    : V
    → ---
      Ty V

  _⇒_
    : (τ : Ty V) (σ : Ty V)
    → ---------------------
      Ty V

-- Convention: τ₁ ⇒ τ₂ ⇒ ... τₙ ≡ τ₁ ⇒ (τ₂ ⇒ (... ⇒ τₙ))
infixr 7 _⇒_

------------------------------------------------------------------------------
-- Context in STLC

Cxt : Type → Type
Cxt V = List (Ty V)

variable
  V   : Type
  τ σ : Ty V
  Γ Δ : Cxt V
  
------------------------------------------------------------------------------
-- Intrinsically-typed de Bruijn representation of STLC
--

infix 4 _⊢_
data _⊢_ {V : Type} : Cxt V → Ty V → Type where
  `_
    : Γ ∋ τ
    → Γ ⊢ τ
-- \cdot
  _·_
    : Γ ⊢ (τ ⇒ σ)
    → Γ ⊢ τ
    → -------------
      Γ ⊢ σ
-- \Gl-
  ƛ_ 
    : (τ ∷ Γ) ⊢ σ
    → Γ ⊢ τ ⇒ σ

infixl 6 _·_ -- \cdot
infixr 5 ƛ_  -- \Gl

variable
  M N L M₁ M₂ N₁ N₂ : Γ ⊢ τ

-- Try out the following λ-terms

K₁ : {τ σ : Ty V}
  → [] ⊢ τ ⇒ σ ⇒ τ
K₁ = ƛ ƛ ` suc zero

I : (τ : Ty V)
  → [] ⊢ τ ⇒ τ
I τ = {!!}

K₂ : {τ σ : Ty V}
  → [] ⊢ τ ⇒ σ ⇒ σ
K₂ = {!!}

Nat : (τ : Ty V) → Ty V
Nat τ = (τ ⇒ τ) ⇒ τ ⇒ τ

c₂ : (τ : Ty V)
  → [] ⊢ Nat τ
c₂ τ = ƛ ƛ ` suc zero · (` suc zero · ` zero)

c₀ : (τ : Ty V)
  → [] ⊢ Nat τ
c₀ τ = {!!} 

c₁ : (τ : Ty V)
  → [] ⊢ Nat τ
c₁ τ = {!!}

------------------------------------------------------------------------------
-- Variable renaming


-- Define it during defining `rename`
--   1. Abbreviate ({τ : Ty V} → Γ ∋ τ → Δ ∋ τ) to Ren
Ren : {V : Type} → Cxt V → Cxt V → Type
Ren {V} Γ Δ = {τ : Ty V} → Γ ∋ τ → Δ ∋ τ
--   2. Define ext

ext : Ren Γ Δ → Ren (τ ∷ Γ) (τ ∷ Δ)
ext ρ zero    = zero
ext ρ (suc x) = suc (ρ x)

-- 3. Finish `rename`

rename
  : {V : Type} {Γ Δ : Cxt V} 
  → ({τ : Ty V} → Γ ∋ τ → Δ ∋ τ)
  → {τ : Ty V}
  → Γ ⊢ τ → Δ ⊢ τ
rename ρ (` x)   = ` ρ x
rename ρ (M · N) = rename ρ M · rename ρ N
rename ρ (ƛ M)   = ƛ rename (ext ρ) M

inc : Ren Γ (σ ∷ Γ)
inc x = suc x
-- inc = suc

weaken : Γ ⊢ τ → (σ ∷ Γ) ⊢ τ
weaken M = rename suc M

------------------------------------------------------------------------------
-- Substitution

-- Skip to `subst` first and do the following during defining `rename`
--    1. Abbreviate ({τ : Ty V} → Γ ∋ τ → Δ ⊢ τ) to Sub
Sub : {V : Type} → Cxt V → Cxt V → Type
Sub {V} Γ Δ = {τ : Ty V} → Γ ∋ τ → Δ ⊢ τ
--    2. Define exts
exts : Sub Γ Δ
  → Sub (τ ∷ Γ) (τ ∷ Δ)
exts σ zero    = ` zero
exts σ (suc x) = weaken (σ x)
--    3. Finish subst

subst
  : {V : Type} {Γ Δ : Cxt V}
  → Sub Γ Δ
  → {τ : Ty V}
  → Γ ⊢ τ → Δ ⊢ τ
subst σ (` x)   = σ x
subst σ (M · N) = subst σ M · subst σ N
subst σ (ƛ M)   = ƛ subst (exts σ) M

ssubst
  : Γ ⊢ τ
  → τ ∷ Γ ⊢ σ
  → Γ ⊢ σ
ssubst {Γ = Γ} {τ} N M = subst σ₀ M
  where
    σ₀ : Sub (τ ∷ Γ) Γ
    σ₀ zero    = N
    σ₀ (suc x) = ` x

_[_] 
  : τ ∷ Γ ⊢ σ
  → Γ ⊢ τ
  → Γ ⊢ σ
M [ N ] = ssubst N M

------------------------------------------------------------------------------
-- β-reduction

infix 4 _→β1_

data _→β1_ : Γ ⊢ τ → Γ ⊢ τ → Type where
  β-ƛ·
    : -----------------------
      (ƛ M) · N →β1 M [ N ]
  ξ-·₁
    : M₁     →β1 M₂
    → -----------------
      M₁ · N →β1 M₂ · N
  ξ-·₂
    : N₁     →β1 N₂
    → -----------------
      M · N₁ →β1 M · N₂
  ξ-ƛ
    : M₁   →β1 M₂
    → --------------
      ƛ M₁ →β1 ƛ M₂

_ : (K₁ · M) · N →β1 ((ƛ ` suc zero) [ M ]) · N
_ = ξ-·₁ β-ƛ·

------------------------------------------------------------------------------
-- Multi-step β-reduction as a sequence of β-reductions 

infix 4 _→β*_

data _→β*_ : (Γ ⊢ τ) → (Γ ⊢ τ) → Type where
  []
    ---------
    : M →β* M
  _∷_
    : M →β1 N
    → N →β* L
    → ---------
      M →β* L

_ : K₁ · I τ · N →β* (I τ)
_ = {!!} -- ξ-·₁ β-ƛ· ∷ β-ƛ· ∷ []

_ : K₂ · N · I τ →β* (I τ)
_ = {!!}

------------------------------------------------------------------------------
-- Preservation is trivial (why?)

------------------------------------------------------------------------------
-- Neutral and Normal

mutual
  data Neutral : Γ ⊢ τ → Type where
    `_
      : (x : Γ ∋ τ)
      → ---------------
        Neutral (` x)
    _·_
      : Neutral M
      → Normal N
      → --------------------
        Neutral (M · N)

  data Normal : Γ ⊢ τ → Type where
    ↑_
      : Neutral M
      → ----------
        Normal  M
    ƛ_
      : Normal M
      → ------------
        Normal (ƛ M)  

mutual
  normal-soundness : Normal M → ¬ (M →β1 N)
  normal-soundness (↑ M↓) d       = neutral-soundness M↓ d
  normal-soundness (ƛ M↓) (ξ-ƛ d) = normal-soundness M↓ d

  neutral-soundness : Neutral M → ¬ (M →β1 N)
  neutral-soundness (` x)   ()
  neutral-soundness (M↓ · N↓) (ξ-·₁ d) = neutral-soundness M↓ d
  neutral-soundness (M↓ · N↓) (ξ-·₂ d) = normal-soundness N↓ d

normal-completeness : (M : Γ ⊢ τ) → ((N : Γ ⊢ τ) → ¬ (M →β1 N)) → Normal M
normal-completeness (` x)   M↛  = ↑ (` x)
normal-completeness (ƛ M)   ƛM↛ = {!!} 
normal-completeness (M · N) MN↛ with normal-completeness M M↛ | normal-completeness N N↛
   where
     M↛ : ∀ M′ → ¬ (M →β1 M′)
     M↛ M′ M→M′ = {!!}
     N↛ : ∀ N′ → ¬ (N →β1 N′)
     N↛ N′ N→N′ = {!!}
... | ↑ M↓ | N↓ = ↑ (M↓ · N↓)
... | ƛ M↓ | N↓ = ⊥-elim (MN↛ _ β-ƛ·)

data Progress (M : Γ ⊢ τ) : Type where
  done
    : Normal M
    → ----------
      Progress M

  step
    : M →β1 N
    → -------
      Progress M

progress : (M : Γ ⊢ τ) → Progress M
progress M = {!!}
