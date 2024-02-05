module 3-Vectors where

------------------------------------------------
--                                            --
-- Dependently Typed Programming              --
--                                            --
-- Episode 3 : Vectors and Inductive Families --
--                                            --
--           Shin-Cheng Mu                    --
--           FLOLAC,  2014                    --
--                                            --
------------------------------------------------

  {- Courtesy of Conor McBride -}

-- As usual, we start with basic definitions.

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

infixr 50 _×_ _,_

-- Vectors: lists indexed by their lengths

data Vec (A : Set) : ℕ → Set where
 [] : Vec A zero
 _∷_ : {n : ℕ} → A → Vec A n → Vec A (suc n) 

 -- While List defines a datatype inductively,
 -- Vec inductively defines a *family* of types
 --   Vec A 0, Vec A 1, Vec A 2 ....

-- examples

ex0 : Vec ℕ 0
ex0 = {!!}

ex1 : Vec ℕ 1
ex1 = {!!}

ex2 : Vec ℕ 2
ex2 = {!!}

head : {A : Set}{n : ℕ} → {!!}
head = {!!}

tail : {A : Set}{n : ℕ} → {!!}
tail = {!!}

zip : ∀{A B n} → Vec A n → Vec B n → Vec (A × B) n
zip xs ys = {!!}

zip3 : ∀{A B C n} → 
         Vec A n → Vec B n → Vec C n →
           Vec (A × B × C) n
zip3 xs ys zs = {!!}

-- concatenating vectors

_+_ : ℕ → ℕ → ℕ 
zero + n = n
(suc m) + n = suc (m + n)

_++_ : ∀ {A m n} → Vec A m → Vec A n → Vec A (m + n)
xs ++ ys = {!!}

  -- What is it about _+_ which makes _++_ typecheck?

  -- You should have defined these functions in your 
  -- Haskell practicals and know them as well as the
  -- backs of your hands. Right?

doubleL : ∀ {A} → List A → List A
doubleL [] = [] 
doubleL (x ∷ xs) = x ∷ x ∷ xs

interleaveL : ∀ {A} → List A → List A → List A
interleaveL [] ys = ys
interleaveL (x ∷ xs) ys = x ∷ interleaveL ys xs

{- * But let's define the Vec version of double.
     Oops, this cannot be done! Why not?

double : ∀ {A n} → Vec A n → Vec A (n + n)
double [] = []
double (x ∷ xs) = x ∷ {!!}

-}

{- We need to somehow convince Agda that suc (m + n)
   and m + (suc n) are the same. Before we can do that,
   we start with something simpler. 

   Even n is a proof that n is an even number.
-}

data Even : ℕ → Set where
  0even : Even 0    -- 0 is even.
  2+even : ∀ {n} → Even n → Even (suc (suc n))
                    -- if n is even, so is suc (suc n),
    -- and there are no other even numbers!

  -- Let's try proving that 6 is even,

6-even : Even 6
6-even = {!!}

  -- and that if n is even, so is 4 + n.

4+even : ∀ n → Even n → Even (4 + n)
4+even = {!!}

 {- * The following are some more exercises using ⊥.
      Not mandatory, but try if you want to understand
      use of ⊥!
 -}

data ⊥ : Set where

¬ : Set → Set
¬ P = P → ⊥

  -- Of course, if 2+n is not even, nor is n.

¬e2+n→¬en : ∀ n → ¬ (Even (suc (suc n))) → ¬ (Even n)
¬e2+n→¬en n = {!!}

  -- One may also define another datatype serving
  -- as a proof that a number is odd.

data Odd : ℕ → Set where
  1odd : Odd (suc zero)
  2+odd : ∀ {n} → Odd n → Odd (suc (suc n))

  -- But are Odd n and ¬ (Even n) equivalent?
  -- We will see below.
 
  -- This function ⊥-elim shows that if we can 
  -- prove ⊥, we can prove anything. Not surprising that
  -- it is useful.

⊥-elim : ∀ {P : Set} → ⊥ → P
⊥-elim ()

  -- Remove the definition below and try this yourself!

¬even→odd : ∀ n → ¬ (Even n) → Odd n
¬even→odd zero p = ⊥-elim (p 0even)
¬even→odd (suc zero) p = 1odd
¬even→odd (suc (suc n)) p = 2+odd (¬even→odd n (¬e2+n→¬en n p)) 

  -- We can also prove Odd n → ¬(Even n), etc.

-- Propositional Equality
--    m ≡ n is a proof that m equals n!

{- A simpler definition.

data _≡_ {A : Set} : A → A → Set where
  refl : ∀ {x} → x ≡ x
-}

  {- To be compatible with some built-in funcitonalities,
     we have to use the following more general definition.
     The difference does not matter to us in this course -}

data _≡_ {a} {A : Set a} (x : A) : A → Set a where
    refl : x ≡ x

{-# BUILTIN EQUALITY _≡_ #-}
{-# BUILTIN REFL refl #-}

infix 4 _≡_

-- The function that, given m and n, producing a proof
-- that  m + suc n equals suc (m + n).
-- You might not be able to complete +-suc yet. Leave the hole
-- blank. Josh will teach you how to construct +-suc. 

+-suc : ∀ m n → m + suc n ≡ suc (m + n)
+-suc m n = {!!}

-- But try to use +-suc to define double.

double : ∀ {A n} → Vec A n → Vec A (n + n)
double {A} {zero} [] = []
double {A} {suc n} (x ∷ xs) with +-suc n n
... | p rewrite p = x ∷ x ∷ double xs

-- Of course, given m and n, we can produce a proof
-- that m + n ≡ n + m. You might not be able to do it now
-- either.

+-comm : ∀ m n → m + n ≡ n + m
+-comm m n = {!!}

-- But, use +-comm to define interleave.

interleave : ∀ {A m n} → Vec A m → Vec A n → Vec A (m + n)
interleave [] ys = {!!}
interleave {A} {suc m} {n} (x ∷ xs) ys = {!!}