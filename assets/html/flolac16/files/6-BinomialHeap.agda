module 6-BinomialHeap where

  {- Courtesy of Josh Ko -}

-- A least-significant-bit-first representation
-- of binary numbers.
-- For example, decimal 11 = binary 1011
-- is represented as one∷ (one∷ (zero∷ (one∷ nul))).
open import Prelude

data Bin : Set where
  nul   : Bin
  zero∷ : Bin → Bin
  one∷  : Bin → Bin

 -- How to increment a binary number by one?

incr : Bin → Bin
incr nul = one∷ nul
incr (zero∷ bs) = one∷ bs
incr (one∷ bs)  = zero∷ (incr bs)

2* : ℕ → ℕ
2* n = n + n

bin→ℕ : Bin → ℕ
bin→ℕ = {!   !}

  -- if you need 2* or addition, etc,
  -- define your own!

data Bool : Set where
  false : Bool
  true : Bool

data ⊤ : Set where
  tt : ⊤

data _×_ (A B : Set) : Set where
  _,_ : A → B → A × B

infixr 5 _×_ _,_

 {- Given X : ℕ → Set and n : ℕ,
    X ^ n is a sequence of n items,
      X n , X (n-1) , ... , X 1.
 -}

_^_ : (ℕ → Set) → ℕ → Set
X ^ zero    = ⊤
X ^ (suc n) = X n × (X ^ n)

{-
  X ^ 0 = ⊤
  X ^ 1 = X 0 × ⊤
  X ^ 2 = X 1 × X 0 × ⊤
  X ^ 3 = X 2 × X 1 × X 0 × ⊤
        :
-}

  -- Today, Vec is only used as an example,
  -- helping you to understand what X ^ n is.

ex1 : Vec Bool ^ 3
ex1 = {!   !}

  -- Binomial Tree.

data BTree : ℕ → Set where
  node : ∀ {r} → (x : ℕ)
         → (ts : BTree ^ r) → BTree r

   -- m ≤? n is true iff m is less than or equal to n.

_≤?_ : ℕ → ℕ → Bool
m ≤? n = {!   !}

attach : ∀ {r} → BTree r → BTree r
               → BTree (suc r)
attach (node x ts) (node y us) with x ≤? y
... | false = node y (node x ts , us)
... | true  = node x (node y us , ts)


data BHeap' : ℕ → Set where
  nul   : ∀ {n} → BHeap' n
  zero∷ : ∀ {n} → BHeap' (suc n) → BHeap' n
  one_∷ : ∀ {n} → BTree n → BHeap' (suc n) → BHeap' n

ins' : ∀ {n} → BTree n → BHeap' n → BHeap' n
ins' = {!   !}

data BHeap : ℕ → Bin → Set where
  nul   : ∀ {n} → BHeap n nul
  zero∷ : ∀ {n m} → BHeap (suc n) m
          → BHeap n (zero∷ m)
  one_∷ : ∀ {n m} → BTree n → BHeap (suc n) m
          → BHeap n (one∷ m)

 -- inserting a binominal tree into a heap.

ins : ∀ {n m} → BTree n → BHeap n m → BHeap n (incr m)
ins = {!   !}

-- Adding binary numbers

_∔_ : Bin → Bin → Bin
m ∔ n = {!   !}

join : ∀ {i m n} → BHeap i m → BHeap i n
       → BHeap i (m ∔ n)
join = {!   !}
