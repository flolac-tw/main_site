module 2-DepTypesC where

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

exII-2 : {A B : Set} 
         {C : A → B → Set}
         → ((x : A) → (y : B) → C x y)
         → ((y : B) → (x : A) → C x y)
exII-2 f y x = f x y

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
exII-3 ((x , y) , z) = x , (y , z)

exII-4a : ∀ {A : Set} {B C : A → Set}
      → ((y : A) → B y) × ((z : A) → C z)
      → ((x : A) → (B x × C x))
exII-4a (f , g) x = f x , g x

exII-4b : ∀ {A : Set} {B C : A → Set}
      → ((x : A) → (B x × C x))
      → ((y : A) → B y) × ((z : A) → C z)
exII-4b f = (λ y → fst (f y)) , (λ z → snd (f z))

-- Axiom of Choice!

AC : ∀ {A B : Set} {R : A → B → Set} 
     → ((x : A) → Σ B (λ y → R x y))
     → Σ (A → B) (λ f → ((z : A) → R z (f z)))
AC g = ((λ x → fst' (g x)) , (λ z → snd' (g z)))


data _⊹_ (A B : Set) : Set where
  left  : A → A ⊹ B
  right : B → A ⊹ B

data ⊥ : Set where    

¬ : Set → Set
¬ P = P → ⊥
 
postulate
  LEM : (P : Set) → P ⊹ (¬ P)

⊥-elim : ∀ {P : Set} → ⊥ → P
⊥-elim ()

exII-18 : ∀ {A : Set} {B : A → Set}
          → (¬ ((x : A) → ¬ (B x))) → Σ A B 
exII-18 {A} {B} f with LEM (Σ A B)
... | left (x , y) = x , y
... | right ¬xy = ⊥-elim (f (λ x y → ¬xy (x , y)))