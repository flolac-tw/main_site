module 1-Intro where

  {- Courtesy of Conor McBride -}

-- Not all things are the same. This
-- gives us something to care about.

-- Haskell:
--   data Bool = False | True

data Bool : Set where
  false : Bool
  true : Bool

-- Programs often produce different output,
-- given different input.

not : Bool → Bool
not = {!   !}

  -- The type could also be written
  --   not : (b : Bool) → Bool

data ℕ : Set where
  zero : ℕ
  suc  : ℕ → ℕ

{-# BUILTIN NATURAL ℕ #-}

  -- The derivative above allows us to use built-in
  -- abbreviations, e.g. 0 for zero, 1 for suc zero,
  -- 2 for suc (suc zero).

-- Types could be parameterised by types.
 --  data List A = [] | A ∷ List A

data List (A : Set) : Set where
  [] : List A
  _∷_ : A → List A → List A

  -- A is in the scope for the constructors.

infixr 5 _∷_  -- declare the precdence and associativity of _∷_.

ex1 : List ℕ
ex1 = {!   !}   -- the list [1,2,3].

-- Type argument must be given,

id₁ : (A : Set) → A → A
id₁ A x = x

   -- e.g. id₁ Bool true

-- unless declared implicit.

id : {A : Set} → A → A
id x = x

   -- e.g. id true
   --      id {Bool} true

-- It's often important to know whether a
-- list is [] or not.

null : ∀ {A} → List A → Bool
null = {!   !}

-- Some more exercises/reviews on functions on lists.

infixr 5 _++_

length : ∀ {A} → List A → ℕ
length xs = {!   !}

_++_ : ∀ {A} → List A → List A → List A
xs ++ ys = {!   !}

map : ∀ {A B} → (A → B) → List A → List B
map f xs = {!   !}

take : ∀ {A} → ℕ → List A → List A
take n xs = {!   !}

   -- ∀ {A} is a shorter syntax for
   --  {A : τ} when τ can be inferred.
   -- ∀ A is a shorter synatx for
   --  (A : τ) when τ can be inferred.

{- * The following function will be rejected by Agda.
     Why?

head : ∀ {A} → List A → A
head (x ∷ xs) = x
-}

{- * xs ‼ n intendes to extract the nth element of xs.
     The intention is, for example,
         (1 ∷ 2 ∷ 3 ∷ []) ‼ 1  =  2
    (There is a corresponding definition in Haskell called (!!)).
    Try completing the definition.

_‼_ : ∀ {A} → List A → ℕ → A
xs ‼ n = ?

    There will be one case where we don't know what to do, however. -}

-- postulate error : {X : Set} → X

-- Dependent types to the rescue:
--   boolean preconditions.

data ⊤ : Set where    -- data Top = TT
   tt : ⊤

data ⊥ : Set where

   -- It's our first function from a value
   -- to a type.

IsTrue : Bool → Set
IsTrue false = ⊥
IsTrue true  = ⊤

headOk : ∀ {A} → (xs : List A) →
            (IsTrue (not (null xs))) → A
headOk = {!   !}

{- * Use headOk to extract the first component of ex1 -}

headex1 : ℕ
headex1 = {! !}

{- * Can you apply headOk to []? How, or why not? -}
   --- ..... headOk [] (...) ...

last : ∀ {A} → (xs : List A) → IsTrue (not (null xs)) → A
last xs p = {! !}


-- a more complex example

_∨_ : Bool → Bool → Bool
true  ∨ q = true
false ∨ b = b

_∧_ : Bool → Bool → Bool
b ∧ c = {!  !}

somewhere : ∀ {A} → (A → Bool) → List A → Bool
somewhere p [] = false
somewhere p (x ∷ xs) = p x ∨ somewhere p xs

find1st : ∀{A} → (p : A → Bool) → (xs : List A) →
           IsTrue (somewhere p xs) → A
find1st p xs q = {!   !}

-- Equality for ℕ

_==_ : ℕ → ℕ → Bool
m == n = {!  !}

-- Less-than-or-equal-to for ℕ

_≤_ : ℕ → ℕ → Bool
zero ≤ n = true
suc m ≤ zero = false
suc m ≤ suc n = m ≤ n

_<_ : ℕ → ℕ → Bool
m < n = {!  !}


 {- This is a safe version of _‼_, in which we demand that, whenever
    we call index xs n, we must have shown that n < length xs -}

index : ∀ {A} → (xs : List A) → (n : ℕ) → IsTrue (n < length xs) → A
index xs n = {!  !}
