module 7-VerifiedCompiler where

  {- Courtesy of Conor McBride -}

open import Prelude

-- a very simple expression language

data Exp : Set where
  nat : ℕ → Exp
  _∔_ : Exp → Exp → Exp

-- an evaluator

eval : Exp → ℕ
eval (nat n) = n
eval (e₁ ∔ e₂) = eval e₁ + eval e₂

-- bytecode for a stack machine, indexed
-- by initial and final stack hights.

data _⇒_ : ℕ → ℕ → Set where
  SKIP  : ∀ {n} → n ⇒ n
  _SEQ_ : ∀ {l m n} → l ⇒ m → m ⇒ n → l ⇒ n
  PUSH  : ∀ {n} → (x : ℕ) → n ⇒ suc n
  ADD   : ∀ {n} → suc (suc n) ⇒ suc n

-- running code takes us from stacks
-- to stacks

_∘_ : {A B C : Set} → (B → C) → (A → B) → (A → C)
(f ∘ g) x = f (g x)

run : ∀ {m n} → m ⇒ n → Vec ℕ m → Vec ℕ n
run SKIP xs = xs
run (c SEQ d) xs = (run d ∘ run c) xs
run (PUSH x) xs = x ∷ xs
run ADD (x ∷ y ∷ xs) = (x + y) ∷ xs

-- compiling takes expressions to code

comp : Exp → ∀ {n} → n ⇒ suc n
comp (nat n) = {!   !}
comp (e₁ ∔ e₂) = {!   !}

comp-correct : ∀ e {n} (xs : Vec ℕ n) → run (comp e) xs ≡ {!   !}
comp-correct = {!   !}

---

add2 : ∀{n}→ Vec ℕ (suc (suc n)) → Vec ℕ (suc n)
add2 (x ∷ y ∷ xs) = (x + y) ∷ xs

data SemCode : (m n : ℕ) →
               (Vec ℕ m → Vec ℕ n) → Set where
  SKIP : ∀{n} → SemCode n n (λ xs → xs)
  PUSH : ∀{n} → (x : ℕ) →
           SemCode n (suc n) (λ xs → x ∷ xs)
  ADD : ∀ {n} →
        SemCode (suc (suc n)) (suc n) add2
  _SEQ_ : ∀ {l m n f g} →
          SemCode l m f → SemCode m n g →
            SemCode l n (g ∘ f)

infixr 5 _SEQ_

scomp : (e : Exp) → ∀ {n} →
        SemCode n (suc n) (λ xs → {!   !})
scomp (nat n) = {!   !}
scomp (e₁ ∔ e₂) = {!   !}

-- If you suspect that we're only getting
-- away with this because the problem is so
-- easy, then you're right. Indexing is
-- intensional. But it won't be for ever.
