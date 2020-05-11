module 1-Intro where

--------------------------------------------
--                                        --
-- Dependently Typed Programming          --
--                                        --
-- Episode 1 : Syntax, Functions, Values  --
--  and simple usage of dependent types   --
--           Shin-Cheng Mu                --
--           FLOLAC,  2014                --
--                                        --
--------------------------------------------

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
not false = true
not true  = false
  
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
ex1 = 1 ∷ 2 ∷ 3 ∷ []    -- the list [1,2,3].


-- Type argument must be given,

id₁ : (A : Set) → A → A
id₁ A x = x

   -- e.g. id₁ Bool true

{- * Which of the following is correct?
      id₁ ex1
      id₁ ℕ ex1
      id₁ List ex1
      id₁ (List ℕ) ex1
     Use ^C ^N to find out. Explain the result.  -}

-- unless declared implicit.

id : {A : Set} → A → A
id x = x

   -- e.g. id true
   --      id {Bool} true

{- * Which of the following are correct?
      id ex1
      id (List ℕ) ex1
      id {List ℕ} ex1
     Use ^C ^N to find out. Explain the result.  -}

-- Agda may help to prove some of the stuffs in the
-- Type Theory class yesterday.

ex2 : ∀ {A B C : Set} → (A → B → C) → B → A → C
ex2 = {!!}

    -- ∀ {A B C : τ} is an abbreviation of
    -- ∀ {A : τ} {B : τ} {C : τ}

ex3 : ∀ {A B C : Set} → (A → B) → (B → C) → A → C
ex3 = {!!}

ex4 : ∀ {A B C : Set} → 
        (A → B → C) → (A → B) → A → C
ex4 = {!!}

-- It's of course helpful to know whether a
-- list is [] or not.

null : ∀ {A} → List A → Bool
null xs = {!!}

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
    (There is a correesponding definition in Haskell called (!!)). 
    Try completing the definition. 

_‼_ : ∀ {A} → List A → ℕ → A
xs ‼ n = ?

    There will be one case where we don't know what to do, however. -}

-- Dependent types to the rescue.

   -- You can define funny functions whose return
   -- type depends on the input.

ℕorBool : Bool → Set
ℕorBool false = ℕ
ℕorBool true  = Bool

ex : (b : Bool) → ℕorBool b
ex false = zero
ex true = true

--   boolean preconditions.

data ⊤ : Set where    -- data Top = TT
   tt : ⊤

data ⊥ : Set where    

IsTrue : Bool → Set
IsTrue false = ⊥
IsTrue true  = ⊤

   -- Note that IsTrue is a function from a value
   -- to a type.

    -- In Josh's class this will be denoted as a Π-type:
    -- useless : π (b:Bool) ℕorBool

    -- With type-valued functions, we can define headOk.

headOk : ∀ {A} → (xs : List A) →
            (IsTrue (not (null xs))) → A
headOk xs p = {!!}

   --- ..... headOk [] (...) ...

{- * Use headOk to extract the first component of ex1 -}

headex1 : ℕ
headex1 = {!!}  

{- * Can you apply headOk to []? How, or why not? -}

last : ∀ {A} → (xs : List A) → IsTrue (not (null xs)) → A
last xs p = ?

-- a more complex example

_∨_ : Bool → Bool → Bool
true ∨ _ = true
false ∨ b = b

_∧_ : Bool → Bool → Bool
b ∧ c = {!!}

somewhere : ∀ {A} → (A → Bool) → List A → Bool
somewhere p [] = false
somewhere p (x ∷ xs) = p x ∨ somewhere p xs

find1st : ∀{A} → (p : A → Bool) → (xs : List A) →
           IsTrue (somewhere p xs) → A 
find1st p xs q = {!!}

-- Equality for ℕ

_==_ : ℕ → ℕ → Bool
m == n = {!!}

-- Less-than-or-equal-to for ℕ

_≤_ : ℕ → ℕ → Bool
zero ≤ n = true
suc m ≤ zero = false
suc m ≤ suc n = m ≤ n

_<_ : ℕ → ℕ → Bool
m < n = {!!}

-- lengths of lists

length : ∀ {A} → List A → ℕ
length [] = zero
length (x ∷ xs) = suc (length xs) 


 {- This is a safe version of _‼_, in which we demand that, whenever
    we call index xs n, we must have shown that n < length xs -}

index : ∀ {A} → (xs : List A) → (n : ℕ) → IsTrue (n < length xs) → A
index xs n = {!!}

-- Pairs. 

data _×_ (A B : Set) : Set where
  _,_ : A → B → A × B

fst : ∀ {A B} → A × B → A
fst (x , y) = x

snd : ∀ {A B} → A × B → B
snd (x , y) = y

ex5 : {!!}  -- what is the type of ex2?
ex5 = ((2 , true), false)

{- * Extract the components 2, true, and false in ex2,
     using fst, snd, etc -}

{- Define zip such that, for example,
 
     zip (1 ∷ 2 ∷ 3 ∷ []) (true ∷ false ∷ true ∷ []) =
        (1 , true) ∷ (2 , false) ∷ (3 , true) ∷ []

   When the two lists are not equally long, the longer
   one is truncated. 

     zip (1 ∷ 2 ∷ []) (true ∷ false ∷ true ∷ []) =
        (1 , true) ∷ (2 , false) ∷ []
-}
zip : ∀ {A B} → (xs : List A) → (ys : List B) → List (A × B)
zip xs ys = {!!} 

{- The following function zip= is like zip, but insisting
   that the two arguments must have the same length -}

zip= : ∀ {A B} → (xs : List A) → (ys : List B) 
      → IsTrue (length xs == length ys) → List (A × B)
zip= xs ys = {!!}

-- Σ type.

data Σ (A : Set) (B : A → Set) : Set where
  _,_ : (x : A) → B x → Σ A B

fst' : ∀ {A B} → Σ A B → A
fst' (x , y) = x

snd' : ∀ {A B} → (p : Σ A B) → B (fst' p)
snd' (x , y) = y

-- Again, use ℕorBool to form some useless examples.
--  ℕorBool : Bool → Set
--  ℕorBool false = ℕ
--  ℕorBool true  = Bool
-- The type Σ Bool ℕorBool is a pair. The type of its
-- second component is 
--   - ℕ if the *value* of its first component is false, or
--   - Bool if the value of its first component it true.

ex6 : Σ Bool ℕorBool
ex6 = (false , {!!})

ex7 : Σ Bool ℕorBool
ex7 = (true , {!!})

-- What's the use of this? We will see later.

-- Finally, re-do some exercises from the type theory
-- class yesterday.

ex8 : ∀ {A B} → (A × B) → (B × A)
ex8 = {!!}

data _⊹_ (A B : Set) : Set where
  left  : A → A ⊹ B
  right : B → A ⊹ B
    
    -- ⊹ can be keyed in by \ + <space>

ex9 : ∀ {A B} → A ⊹ B → B ⊹ A
ex9 = {!!}

ex10 : ∀ {A B C} → A ⊹ (B ⊹ C) → (A ⊹ B) ⊹ C 
ex10 = {!!}

ex11 : ∀ {A B C} → A ⊹ (B × C) → (A ⊹ B) × (A ⊹ C)
ex11 = {!!}

¬ : Set → Set 
¬ P = P → ⊥
  -- ¬ can be keyed in by \neg

ex12 : ∀ {A} → A → ¬ (¬ A)
ex12 = {!!}

ex13 : ∀ {A B} → (¬ A) ⊹ (¬ B) → ¬ (A × B)
ex13 = {!!}

ex14 : ∀ {A B} → (A → B) → (¬ B → ¬ A)
ex14 = {!!}
