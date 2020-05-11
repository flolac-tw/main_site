module Prelude where

-- Again we start with natural numbers.

data ℕ : Set where
  zero : ℕ
  suc : ℕ → ℕ

{-# BUILTIN NATURAL ℕ #-}
-- for Agda < 2.4.0
-- {-# BUILTIN ZERO zero #-}
-- {-# BUILTIN SUC  suc  #-}

_+_ : ℕ → ℕ → ℕ
zero + n = n
(suc m) + n = suc (m + n)

-- and lists.

data List (A : Set) : Set where
  [] : List A
  _∷_ : A → List A → List A

infixr 5 _∷_

data Vec (A : Set) : ℕ → Set where
 [] : Vec A zero
 _∷_ : {n : ℕ} → A → Vec A n → Vec A (suc n)


-- The following datatype _≡_, also called "propositional equality",
-- relates two terms that are "provably equal" -- that is, x ≡ y has
-- a proof only if it can be shown that x and y expands to the same
-- term.

data _≡_ {l}{A : Set l} : A → A → Set l where
  refl : ∀ {x} → x ≡ x

infix 4 _≡_

-- The only constructor of _≡_ is "refl", which merely says that
-- x ≡ x for all x.

{-# BUILTIN EQUALITY _≡_ #-}
{-# BUILTIN REFL refl #-}

sym : {A : Set} {x y : A} → x ≡ y → y ≡ x
sym refl = refl

trans : {A : Set} {x y z : A} → x ≡ y → y ≡ z → x ≡ z
trans refl y≡z = y≡z

 -- subst is transport in the MLTT class.
subst : {A : Set} (P : A → Set)
      → ∀ {x y} → x ≡ y → P x → P y
subst P refl p = p

cong : {A B : Set} {x y : A} (f : A → B) → x ≡ y → f x ≡ f y
cong f refl = refl

-- infix  4 _IsRelatedTo_
infix  3 _□
infixr 2 _≡⟨_⟩_
-- infix  1 begin_

_□ : ∀ {A : Set} (x : A) → x ≡ x
x □ = refl

_≡⟨_⟩_ : ∀ {A : Set} → (x : A) → {y z : A}
         → x ≡ y → y ≡ z → x ≡ z
x ≡⟨ x≡y ⟩ y≡z = trans x≡y y≡z
