module 3-Logic where

-- In this practical we play around with Curry-Howard
-- isomorphism in Agda.

-- Implication is encoded by function space.

I : {P : Set} → P → P
I x = {!   !}

K : {P Q : Set} → P → Q → P
K x y = {!   !}

S : {P Q R : Set} → (P → Q → R) → (P → Q) → P → R
S f g x = {!   !}

-- True is encoded by the type ⊤, a type with exactly
-- one component tt.

data ⊤ : Set where
  tt : ⊤

-- False is represented by ⊥, a type having no terms.

data ⊥ : Set where

-- Negation of P is P → False.

¬ : Set → Set
¬ P = P → ⊥

-- Conjunction P ∧ Q is encoded by a pair P × Q.

data _∧_ (A B : Set) : Set where
  _,_ : A → B → A ∧ B

infixr 4 _,_
infixr 2 _∧_

fst : {A B : Set} → A ∧ B → A
fst x = {!   !}

snd : {A B : Set} → A ∧ B → B
snd x = {!   !}

-- Disjunction P ∨ Q is encoded by the sum type.

data _∨_ (A B : Set) : Set where
  inj₁ : A → A ∨ B
  inj₂ : B → A ∨ B

infixr 1 _∨_

-- Exercises from your Logic class!

∨-comm : {A B : Set} → (A ∨ B) → (B ∨ A)
∨-comm = {!   !}

∧-comm : {A B : Set} → (A ∧ B) → (B ∧ A)
∧-comm = {!   !}

→-∨-weakening-r : {A B C : Set} → (A → B) → (A → (B ∨ C))
→-∨-weakening-r = {!   !}

→-∨-weakening-l : {A B C : Set} → ((A ∨ C) → B) → (A → B)
→-∨-weakening-l = {!   !}

→-∧-weakening-r1 : {A B C : Set} → (A → (B ∧ C)) → (A → B)
→-∧-weakening-r1 = {!   !}

→-∧-weakening-r2 : {A B C : Set} → (A → (B ∧ C)) → (A → C)
→-∧-weakening-r2 = {!   !}

→-∧-distr : {A B C : Set} → (A → (B ∧ C)) → ((A → B) ∧ (A → C))
→-∧-distr = {!   !}

⊥-elim : {A : Set} → ⊥ → A
⊥-elim = {!   !}

resol : {A B : Set} → ((A ∨ B) ∧ ¬ B) → A
resol = {!   !}

¬¬ex-middle : {A : Set} → ¬ (¬ (A ∨ ¬ A))
¬¬ex-middle = {!   !}

{- However, we cannot prove that:

ex-middle : {A : Set} → A ∨ (¬ A)
ex-middle = ?
-}

A→¬¬A : {A : Set} → A → ¬ (¬ A)
A→¬¬A = {!   !}


{- However, we cannot prove that:

¬¬A→A : {A : Set} → ¬ (¬ A) → A
¬¬A→A = ?
-}

demorgan : {A B : Set} → (¬ A ∨ ¬ B) → ¬ (A ∧ B)
demorgan = {!   !}

{- However, we cannot prove that

demorgan' : {A B : Set} → ¬ (A ∧ B) → (¬ A ∨ ¬ B)
demorgan' = ?

-}

contra : {A B : Set} → (A → B) → (¬ B → ¬ A)
contra = {!   !}

{- However, we cannot prove that

contra' : {A B : Set} → (¬ B → ¬ A) → (A → B)
contra' = ?
-}

-- Some exercises from MLTT...

-- Π type.

flip : {A B : Set} {C : A → B → Set}
     → ((x : A) → (y : B) → C x y)
     → (y : B) → (x : A) → C x y
flip = {!   !}

-- Recall Bool and ℕ, to be used in examples later.

data Bool : Set where
  false : Bool
  true : Bool

data ℕ : Set where
  zero : ℕ
  suc  : ℕ → ℕ

{-# BUILTIN NATURAL ℕ #-}

-- Σ type.

data Σ (A : Set) (B : A → Set) : Set where
  _,_ : (x : A) → B x → Σ A B

 -- With Σ we can construct things such as

BoolOrℕ : Set
BoolOrℕ = Σ Bool (λ b → if b then Bool else ℕ)
  where if_then_else : Bool → Set → Set → Set
        if true  then P else Q = P
        if false then P else Q = Q

pair1 : BoolOrℕ
pair1 = (true , false)

pair2 : BoolOrℕ
pair2 = (false , 0)

  -- fst and snd can also be defined for Σ.
fst' : ∀ {A B} → Σ A B → A
fst' (x , y) = x

  -- but what should the type of snd' be?
snd' : ∀ {A B} → (p : Σ A B) → B {!   !}
snd' (x , y) = {!   !}

Σ-∧-assoc : {A B : Set} {C : A → B → Set}
          → Σ (A ∧ B) (λ p → C (fst p) (snd p))
          → Σ A (λ x → Σ B (λ y → C x y))
Σ-∧-assoc = {!   !}

curry : {A : Set} {B C : A → Set}
      → ((p : Σ A B) → C (fst' p))
      → (x : A) → B x → C x
curry = {!   !}

→-∧-distr⇒ : {A : Set} {B C : A → Set}
          → ((x : A) → (B x ∧ C x))
          → ((y : A) → B y) ∧ ((z : A) → C z)
→-∧-distr⇒ = {!   !}

→-∧-distr⇐ : {A : Set} {B C : A → Set}
          → ((y : A) → B y) ∧ ((z : A) → C z)
          → ((x : A) → (B x ∧ C x))
→-∧-distr⇐ = {!   !}

Σ-∨-distr⇒ : {A : Set} {B C : A → Set}
           → Σ A (λ x → B x ∨ C x)
           → Σ A (λ y → B y) ∨ Σ A (λ z → C z)
Σ-∨-distr⇒ = {!   !}

Σ-∨-distr⇐ : {A : Set} {B C : A → Set}
           → Σ A (λ y → B y) ∨ Σ A (λ z → C z)
           → Σ A (λ x → B x ∨ C x)
Σ-∨-distr⇐ = {!   !}

→-∨-distr⇐ : {A : Set} {B C : A → Set}
           → ((y : A) → B y) ∨ ((z : A) → C z)
           → ((x : A) → (B x ∨ C x))
→-∨-distr⇐ = {!   !}

-- Can this be proved?
→-∨-distr⇒ : {A : Set} {B C : A → Set}
           → ((x : A) → (B x ∨ C x))
           → ((y : A) → B y) ∨ ((z : A) → C z)
→-∨-distr⇒ = {!   !}

Σ-∧-distr⇒ : {A : Set} {B C : A → Set}
           → Σ A (λ x → B x ∧ C x)
           → Σ A (λ y → B y) ∧ Σ A (λ z → C z)
Σ-∧-distr⇒ = {!   !}

-- Can this be proved?
Σ-∧-distr⇐ : {A : Set} {B C : A → Set}
           → Σ A (λ y → B y) ∧ Σ A (λ z → C z)
           → Σ A (λ x → B x ∧ C x)
Σ-∧-distr⇐ = {!   !}

choice : {A B : Set}{R : A → B → Set}
       → ((x : A) → Σ B (λ y → R x y))
       → Σ (A → B) (λ f → (z : A) → R z (f z))
choice = {!   !}

¬→¬ : {A : Set} {B : A → Set}
    → ¬ ((x : A) → ¬ (B x)) → Σ A (λ y → B y)
¬→¬ = {!   !}
   where postulate
           LEM : {X : Set} → X ∨ ¬ X
