module 2-DepTypes where

{-
 Episode 2. 
 
 Some exercises from the Type Theory class. Or,
   why don't you ask the Magic Agda?
 
 -- Translation from type-theory notations to Agda --
 
 * A : U (where U is the small universe) is 
   translate to A : Set.

 * Π [x : A] B  , where x may appear in B, is 
   translated to 
     (x : A) → B

 * Σ [x : A] B  , where x may appear in B, is
   translated to 
     Σ A (λ x → B)
-}

exII-2 : ∀ {A B : Set} {C : A → B → Set} 
      → ((x : A) → (y : B) → C x y)
      → ((y : B) → (x : A) → C x y)
exII-2 = {!!}

data _×_ (A B : Set) : Set where
  _,_ : A → B → A × B

fst : ∀ {A B} → A × B → A
fst (x , y) = x

snd : ∀ {A B} → A × B → B
snd (x , y) = y

infixr 50 _×_ _,_

data Σ (A : Set) (B : A → Set) : Set where
  _,_ : (x : A) → B x → Σ A B

fst' : ∀ {A B} → Σ A B → A
fst' (x , y) = x

snd' : ∀ {A B} → (p : Σ A B) → B (fst' p)
snd' (x , y) = y

exII-3 : ∀ {A B : Set} {C : A → B → Set} 
      → Σ (A × B) (λ p → C (fst p) (snd p))
      → Σ A (λ x → (Σ B (λ y → C x y)))
exII-3 = {!!}

exII-4a : ∀ {A : Set} {B C : A → Set}
      → ((y : A) → B y) × ((z : A) → C z)
      → ((x : A) → (B x × C x))
exII-4a = {!!}

exII-4b : ∀ {A : Set} {B C : A → Set}
      → ((x : A) → (B x × C x))
      → ((y : A) → B y) × ((z : A) → C z)
exII-4b = {!!}

-- Axiom of Choice!

AC : ∀ {A B : Set} {R : A → B → Set} 
     → ((x : A) → Σ B (λ y → R x y))
     → Σ (A → B) (λ f → ((z : A) → R z (f z)))
AC g = let f x = fst' (g x)
       in (f , (λ z → snd' (g z)))