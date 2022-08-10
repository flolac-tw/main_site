{-# OPTIONS --no-import-sorts #-}
module Lambda where

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
