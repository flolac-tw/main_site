module 2-IndFamilies where

  {- Courtesy of Conor McBride -}

data ℕ : Set where
  zero : ℕ
  suc : ℕ → ℕ

{-# BUILTIN NATURAL ℕ #-}
  -- The derivative above allows us to use built-in
  -- abbreviations, e.g. 0 for zero, 1 for suc zero,
  -- 2 for suc (suc zero).

data List (A : Set) : Set where
  [] : List A
  _∷_ : A → List A → List A

infixr 5 _∷_

  -- any connection between ℕ and List A?

length : ∀ {A} → List A → ℕ
length [] = zero
length (x ∷ xs) = suc (length xs)

data _×_ (A B : Set) : Set where
  _,_ : A → B → A × B

infixr 5 _×_ _,_

-- Vectors: lists indexed by their lengths

data Vec (A : Set) : ℕ → Set where
 [] : Vec A zero
 _∷_ : {n : ℕ} → A → Vec A n → Vec A (suc n)

 -- While List defines a datatype inductively,
 -- Vec inductively defines a *family* of types
 --   Vec A 0, Vec A 1, Vec A 2 ....

head : ∀ {A}{n} → Vec A (suc n) → A
head (x ∷ xs) = {!   !}

tail : ∀ {A}{n} → Vec A (suc n) → Vec A n
tail (x ∷ xs) = {!   !}

zip : ∀{A B n} → Vec A n → Vec B n → Vec (A × B) n
zip = {!   !}

zip3 : ∀{A B C n} →
         Vec A n → Vec B n → Vec C n →
           Vec (A × B × C) n
zip3 = {!   !}

map : ∀{A B n} → (A → B) → Vec A n → Vec B n
map = {!   !}

pure : ∀ {A n} → A → Vec A n
pure {A} {zero} x = []
pure {A} {suc n} x = x ∷ pure x

-- concatenating vectors

_+_ : ℕ → ℕ → ℕ
zero + n = n
(suc m) + n = suc (m + n)

_++_ : ∀ {A m n} → Vec A m → Vec A n → Vec A (m + n)
xs ++ ys = {!   !}

  -- What is it about _+_ which makes _++_ typecheck?

data Even : ℕ → Set where
  ze : Even zero
  sse : ∀ {n} → Even n → Even (suc (suc n))

6-even : Even 6
6-even = {!   !}

sse→e : ∀ {n} → Even (suc (suc n)) → Even n
sse→e = {!   !}

e+e→e : ∀ {m n} → Even m → Even n → Even (m + n)
e+e→e = {!   !}

data ⊥ : Set where

¬ : Set → Set
¬ P = P → ⊥

¬e2+n→¬en : ∀ {n} → ¬ (Even (suc (suc n))) → ¬ (Even n)
¬e2+n→¬en {n} ¬e2+n en = {!   !}

⊥-elim : ∀ {P : Set} → ⊥ → P
⊥-elim ()

data Odd : ℕ → Set where
  zo : Odd (suc zero)
  sso : ∀ {n} → Odd n → Odd (suc (suc n))

¬even→odd : ∀ {n} → ¬ (Even n) → Odd n
¬even→odd = {!   !}
