module 4-Membership where

-- In this practical we emphasize more on the idea that
-- a proof is a program/term.

open import 3-Logic

-- Define lists again.

data List (A : Set) : Set where
  [] : List A
  _∷_ : A → List A → List A

infixr 5 _∷_

infix 4 _∈_

-- What does it mean if we say `x is a member of xs`?
--  1. x is a member of x ∷ xs.
--  2. If x is a member of xs, x is a member of y ∷ xs.
-- The two axioms above are expressed as a datatype.

data _∈_ {A : Set} : A → List A → Set where
  here  : ∀ {x xs} → x ∈ (x ∷ xs)
  there : ∀ {x y xs} → x ∈ xs → x ∈ (y ∷ xs)

infixr 5 _++_

-- Recall (++).

_++_ : ∀ {A} → List A → List A → List A
[] ++ ys = ys
(x ∷ xs) ++ ys = x ∷ (xs ++ ys)

-- Try proving the following properties.

∈-++-pos : ∀ {A : Set} {x : A} xs ys → x ∈ xs ++ ys
           → (x ∈ xs) ∨ (x ∈ ys)
∈-++-pos = {!   !}

∈-++-r : ∀ {A : Set} {x : A} xs {ys}
           → x ∈ ys → x ∈ (xs ++ ys)
∈-++-r = {!   !}

∈-++-l : ∀ {A : Set} {x : A} {xs} ys
           → x ∈ xs → x ∈ (xs ++ ys)
∈-++-l = {!   !}

∈-++-weaken : ∀ {A : Set} {x : A} xs ys zs
              → x ∈ (xs ++ zs) → x ∈ (xs ++ ys ++ zs)
∈-++-weaken = {!   !}

infix 4 _∉_

_∉_ : ∀ {A} → A → List A → Set
x ∉ xs = ¬ (x ∈ xs)

∉-++-l : {A : Set} {x : A} (xs ys : List A) → x ∉ xs ++ ys → x ∉ xs
∉-++-l = {!   !}

∉-++-r : {A : Set} {x : A} (xs ys : List A) → x ∉ xs ++ ys → x ∉ ys
∉-++-r = {!   !}

∉-++-join : ∀ {A : Set} {x : A}
            → ∀ xs ys → x ∉ xs → x ∉ ys → x ∉ xs ++ ys
∉-++-join = {!   !}

∉-++-weaken : ∀ {A : Set} {x : A} xs ys zs
              → x ∉ (xs ++ ys ++ zs) → x ∉ (xs ++ zs)
∉-++-weaken = {!   !}
