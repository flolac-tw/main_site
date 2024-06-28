module 4-BinomialHeap where

------------------------------------------------
--                                            --
-- Dependently Typed Programming              --
--                                            --
-- Episode 4 : Binomial Heaps                 --
--                                            --
--           Shin-Cheng Mu                    --
--           FLOLAC,  2014                    --
--                                            --
------------------------------------------------

  {- Courtesy of Josh Ko -}

-- A least-significant-bit-first representation
-- of binary numbers.
-- For example, decimal 11 = binary 1011 
-- is represented as one∷ (one∷ (zero∷ (one∷ nul))).

data Bin : Set where
  nul   : Bin
  zero∷ : Bin → Bin
  one∷  : Bin → Bin

 -- How to increment a binary number by one?

incr : Bin → Bin
incr nul = one∷ nul
incr (zero∷ bs) = one∷ bs
incr (one∷ bs)  = zero∷ (incr bs)

data ℕ : Set where
  zero : ℕ
  suc  : ℕ → ℕ

{-# BUILTIN NATURAL ℕ #-}
-- for Agda < 2.4.0
-- {-# BUILTIN ZERO zero #-}
-- {-# BUILTIN SUC  suc  #-}

bin→ℕ : Bin → ℕ
bin→ℕ = {!!}  
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

data Vec (A : Set) : ℕ → Set where
 [] : Vec A zero
 _∷_ : {n : ℕ} → A → Vec A n → Vec A (suc n) 

infixr 5 _∷_

ex1 : Vec Bool ^ 3
ex1 = {!!}

  -- Binomial Tree.

data BTree : ℕ → Set where
  node : ∀ {r} → (x : ℕ) 
         → (ts : BTree ^ r) → BTree r

   -- m ≤? n is true iff m is less than or equal to n.

_≤?_ : ℕ → ℕ → Bool
m ≤? n = {!!}

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
ins' t ts = {!!}

data BHeap : ℕ → Bin → Set where
  nul   : ∀ {n} → BHeap n nul
  zero∷ : ∀ {n m} → BHeap (suc n) m
          → BHeap n (zero∷ m)
  one_∷ : ∀ {n m} → BTree n → BHeap (suc n) m
          → BHeap n (one∷ m)

 -- inserting a binominal tree into a heap.

ins : ∀ {n m} → BTree n → BHeap n m → BHeap n (incr m)
ins t ts = {!!}

-- Adding binary numbers

_∔_ : Bin → Bin → Bin
m ∔ n = {!!}

  -- merging two heaps.

join : ∀ {i m n} → BHeap i m → BHeap i n 
       → BHeap i (m ∔ n)
join ts us = {!!}

