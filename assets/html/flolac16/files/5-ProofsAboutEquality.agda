module 5-ProofsAboutEquality where

open import Prelude

infixr 5 _++_

_++_ : ∀ {A} → List A → List A → List A
[] ++ ys = ys
(x ∷ xs) ++ ys = x ∷ (xs ++ ys)

length : ∀ {A} → List A → ℕ
length [] = zero
length (x ∷ xs) = suc (length xs)

-- Let's try proving the theorem
--   length (xs ++ ys) = length xs + length ys
-- But how do you talk about equality in Agda?

-- See _≡_ in Prelude.agda.

-- The proposition we want to prove:
--    ∀{A} (xs ys : List A) → length (xs ++ ys) ≡ length xs + length ys
-- Of course, the first thing to do is to do case analysis on xs.
-- Afterwards, there are several ways to proceed.

-- 0. Use congruence.

length-++-0 : ∀{A} (xs ys : List A) → length (xs ++ ys) ≡ length xs + length ys
length-++-0 [] ys = refl
length-++-0 (x ∷ xs) ys = cong suc (length-++-0 xs ys)

-- 1. Use rewrite.

length-++-1 : ∀{A} (xs ys : List A) → length (xs ++ ys) ≡ length xs + length ys
length-++-1 [] ys = refl
length-++-1 (x ∷ xs) ys rewrite length-++-1 xs ys = refl

-- 2. Use equational reasoning (and cong).

length-++-2 : ∀{A} (xs ys : List A) → length (xs ++ ys) ≡ length xs + length ys
length-++-2 [] ys = refl
length-++-2 (x ∷ xs) ys =
    length ((x ∷ xs) ++ ys)
  ≡⟨ refl ⟩
    length (x ∷ xs ++ ys)
  ≡⟨ refl ⟩
    suc (length (xs ++ ys))
  ≡⟨ cong suc (length-++-2 xs ys) ⟩
    suc (length xs + length ys)
  ≡⟨ refl ⟩
    suc (length xs) + length ys
  ≡⟨ refl ⟩
    length (x ∷ xs) + length ys □

+-suc : ∀ m n → m + suc n ≡ suc (m + n)
+-suc zero n = refl
+-suc (suc m) n rewrite +-suc m n = refl

+-zero : ∀ n → n ≡ n + zero
+-zero zero = refl
+-zero (suc n) = cong suc (+-zero n)

+-comm : ∀ m n → m + n ≡ n + m
+-comm zero n = +-zero n
+-comm (suc m) n rewrite +-suc n m | +-comm m n = refl

+-assoc : ∀ m n k → m + (n + k) ≡ (m + n) + k
+-assoc zero n k = refl
+-assoc (suc m) n k = cong suc (+-assoc m n k)

sum : List ℕ → ℕ
sum [] = 0
sum (x ∷ xs) = x + sum xs

map : ∀{A B} → (A → B) → List A → List B
map f [] = []
map f (x ∷ xs) = f x ∷ map f xs

concat : ∀ {A} → List (List A) → List A
concat [] = []
concat (xs ∷ xss) = xs ++ concat xss

sum-++ : ∀ xs ys → sum (xs ++ ys) ≡ (sum xs + sum ys)
sum-++ = {!   !}

sum-concat : ∀ xss → sum (concat xss) ≡ sum (map sum xss)
sum-concat = {!   !}

take : ∀ {A} → ℕ → List A → List A
take zero xs = []
take (suc n) [] = []
take (suc n) (x ∷ xs) = x ∷ take n xs

drop : ∀ {A} → ℕ → List A → List A
drop zero xs = xs
drop (suc n) [] = []
drop (suc n) (x ∷ xs) = drop n xs

take-drop : ∀ {A} n (xs : List A) → take n xs ++ drop n xs ≡ xs
take-drop = {!   !}

-- Vectors: lists indexed by their lengths

double : ∀ {A n} → Vec A n → Vec A (n + n)
double = {!   !}

interleave : ∀ {A m n} → Vec A m → Vec A n → Vec A (m + n)
interleave = {!   !}
