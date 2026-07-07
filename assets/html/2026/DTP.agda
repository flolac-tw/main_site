--------------------------------------------------------------------------------
--
---- Logic V: A taste of dependently typed programming in Agda
--
---- Josh Ko (Institute of Information Science, Academia Sinica, Taiwan)
--
--------------------------------------------------------------------------------

variable A B C : Set

--------------------------------------------------------------------------------
--
---- Introducing propositional connectives in Agda
--
-- The NJ introduction rules of connectives define the meaning of the
-- connectives by specifying the canonical proofs of the connectives.
-- They correspond to the types of the constructors in datatype definitions
-- in Agda.
--
-- Conjunction corresponds to product/pair types:

data _×_ (A B : Set) : Set where
  _,_ : A → B → A × B

infix 3 _×_

-- Disjunction corresponds to binary sum types:

data _⊎_ (A B : Set) : Set where
  inl : A → A ⊎ B
  inr : B → A ⊎ B

infix 3 _⊎_

-- Implication corresponds to function types, which are primitive in Agda and
-- do not require a definition.
--
-- Falsity corresponds to an empty datatype that has no constructors:

data ⊥ : Set where

¬_ : Set → Set
¬ A = A → ⊥

-- For convenience, we can model truth as a datatype with one constructor:

data ⊤ : Set where
  tt : ⊤

--
--------------------------------------------------------------------------------


--------------------------------------------------------------------------------
--
---- Eliminating propositional connectives in Agda
--
-- The NJ elimination rules can be defined by pattern matching, which is
-- the primitive elimination form in Agda (differing from conventional
-- Type Theory).

outl : A × B → A
outl (a , b) = a

outr : A × B → B
outr (a , b) = b

case : A ⊎ B → (A → C) → (B → C) → C
case (inl a) f g = f a
case (inr b) f g = g b

abort : ⊥ → A
abort ()

-- We can now prove logical theorems in Agda using the constructors
-- of the logical datatypes (corresponding to the introduction rules)
-- and the eliminators above (corresponding to the elimination rules).

distr : A × (B ⊎ C) → (A × B) ⊎ (A × C)
distr = λ x → case (outr x) (λ b → inl (outl x , b)) (λ c → inr (outl x , c))

-- Note the similarity between constructing a program in Agda (interactively)
-- and constructing a derivation in NJ.
--
--------------------------------------------------------------------------------


--------------------------------------------------------------------------------
--
---- Exploiting the full power of Agda
--
-- As long as the important meta-theoretic properties (e.g., consistency and
-- canonicity) still hold, we don’t have to restrict ourselves to the NJ
-- eliminators, and can use more convenient programming constructs offered by
-- Agda (in particular pattern matching).

distr' : A × (B ⊎ C) → (A × B) ⊎ (A × C)
distr' (a , inl b) = inl (a , b)
distr' (a , inr c) = inr (a , c)

--
--------------------------------------------------------------------------------


--------------------------------------------------------------------------------
--
---- Predicates
--
-- In Agda, a predicate is represented as a type function P : A → Set, which,
-- to each element x : A, assigns a type P x : Set describing what proof is
-- required for saying that x satisfies the predicate/property.  That is,
-- x satisfies P exactly when we can construct a program of type P x.
--
-- Example:

data ℕ : Set where
  zero : ℕ
  suc  : ℕ → ℕ

{-# BUILTIN NATURAL ℕ #-}  -- for literals

Even : ℕ → Set
Even zero          = ⊤
Even (suc zero)    = ⊥
Even (suc (suc n)) = Even n

Odd : ℕ → Set
Odd zero          = ⊥
Odd (suc zero)    = ⊤
Odd (suc (suc n)) = Odd n

even-42 : Even 42
even-42 = tt

¬odd-42 : ¬ Odd 42
¬odd-42 ()

-- Note that there is some computation going on at type level (which expands
-- the definitions of Even and Odd).  More generally speaking, think of Agda’s
-- type checking as being performed on the normal form of types.
--
--------------------------------------------------------------------------------


--------------------------------------------------------------------------------
--
---- Universal quantification as dependent function types
--
-- A proof that a predicate P : A → Set holds for all x : A is a function
-- that produces a proof of P x upon receiving any x : A.  Note that in the
-- type of the function, the return type P x depends on its input x:
--
--   (x : A) → P x
--
-- This is exactly a dependent function type in Agda.  (It is also called
-- a Π-type since a function of the above type can be seen as an element
-- of the product of the A-indexed family of types { P x | x ∈ A }.)
--
-- Example: we can state and prove the proposition ‘every natural number is
-- either even or odd’ as follows:

even-or-odd : (n : ℕ) → Even n ⊎ Odd n
even-or-odd zero          = inl tt
even-or-odd (suc zero)    = inr tt
even-or-odd (suc (suc n)) = even-or-odd n

-- From this example we see that a proof by induction is just a structurally
-- recursive program in Agda.
--
--------------------------------------------------------------------------------


--------------------------------------------------------------------------------
--
---- Existential quantification as dependent pair types
--
-- A proof that a predicate P : A → Set holds for some x : A is a pair whose
-- first component is an element x : A and whose second component is a proof
-- of P x.  Note that the type of the second component depends on (the value of)
-- the first component.  These dependent pair types are defined in Agda as

data Σ (A : Set) (P : A → Set) : Set where
  _,_ : (x : A) → P x → Σ A P

infix  2 Σ
infixr 4 _,_

-- We can make the syntax more natural in Agda with the following declaration:

syntax Σ A (λ x → M) = Σ[ x ∶ A ] M

-- Dependent pair types are named Σ-types because a dependent pair can be seen
-- as an element of the sum of the A-indexed family of types { P x | x ∈ A }.
--
-- Example: we can state and prove the proposition ‘there exists an even
-- natural number’ as follows:

exists-even : Σ[ n ∶ ℕ ] Even n
exists-even = 42 , even-42

--
--------------------------------------------------------------------------------


--------------------------------------------------------------------------------
--
---- The Curry–Howard distinguisher
--
-- To determine whether someone thinks about predicate logic in the Curry–Howard
-- way, ask them why the following two propositions are equivalent:
--
--   ∀x. (P x → Q)    and    (∃x. P x) → Q
--
-- To the Curry–Howard eye, the two propositions are immediately recognised as
-- a curried function type and its uncurried version.

uncurry : {P : A → Set} {Q : Set}
        → ((x : A) → P x → Q) → ((Σ[ x ∶ A ] P x) → Q)
uncurry f (x , p) = f x p

curry : {P : A → Set} {Q : Set}
      → ((Σ[ x ∶ A ] P x) → Q) → ((x : A) → P x → Q)
curry f x p = f (x , p)

--
--------------------------------------------------------------------------------


--------------------------------------------------------------------------------
--
---- Equality types
--
-- The equality predicate _≡_ on two elements of type A is defined such that
-- x ≡ y is inhabited (by refl) exactly when x and y have the same normal form.

data _≡_ {A : Set} : A → A → Set where
  refl : {x : A} → x ≡ x

infix 2 _≡_

-- For example, defining addition of natural numbers as

_+_ : ℕ → ℕ → ℕ
zero  + n = n
suc m + n = suc (m + n)

infixr 5 _+_

-- Agda can directly confirm that 1 + 1 ≡ 2 — simple things should be simple
-- to prove!

principia : 1 + 1 ≡ 2
principia = refl

-- On the other hand, the type 1 + 1 ≡ 3 is uninhabited: Agda knows that
-- zero and suc are never equal, and can immediately confirm that there’s
-- no proof of 1 + 1 ≡ 3 upon instruction.

1+1≢3 : ¬ (1 + 1 ≡ 3)
1+1≢3 ()

--
--------------------------------------------------------------------------------

cong : (f : A → B) {x y : A} → x ≡ y → f x ≡ f y
cong f refl = refl

--------------------------------------------------------------------------------
--
---- Equational reasoning combinators
--
-- To make equational proofs human-readable, we can use the following cleverly
-- designed combinators:

begin_ : {x y : A} → x ≡ y → x ≡ y
begin eq = eq

_≡⟨_⟩_ : (x {y z} : A) → x ≡ y → y ≡ z → x ≡ z
x ≡⟨ refl ⟩ y≡z = y≡z

_∎ : (x : A) → x ≡ x
x ∎ = refl

infix  1 begin_
infixr 2 _≡⟨_⟩_
infix  3 _∎

-- With the combinators, we can write equational proofs in a style that is much
-- closer to what we write on paper; moreover, Agda checks the validity of the
-- proofs for us!

data List (A : Set) : Set where
  []  :              List A
  _∷_ : A → List A → List A

sum : List ℕ → ℕ
sum []       = 0
sum (x ∷ xs) = x + sum xs

map : (A → B) → List A → List B
map f [] = []
map f (x ∷ xs) = f x ∷ map f xs

sum∘map : (f : ℕ → ℕ)
        → Σ[ g ∶ (List ℕ → ℕ) ] ((xs : List ℕ) → g xs ≡ sum (map f xs))
sum∘map f = (g , g-correctness) where

  g : List ℕ → ℕ
  g []       = 0
  g (x ∷ xs) = f x + g xs

  g-correctness : (xs : List ℕ) → g xs ≡ sum (map f xs)
  g-correctness [] =
    begin
      g []
        ≡⟨ refl ⟩
      0
        ≡⟨ refl ⟩
      sum []
        ≡⟨ refl ⟩
      sum (map f [])
    ∎
  g-correctness (x ∷ xs) =
    begin
      g (x ∷ xs)
        ≡⟨ refl ⟩
      f x + g xs
        ≡⟨ cong (f x +_) (g-correctness xs) ⟩
      f x + sum (map f xs)
        ≡⟨ refl ⟩
      sum (f x ∷ map f xs)
        ≡⟨ refl ⟩
      sum (map f (x ∷ xs))
    ∎

--
--------------------------------------------------------------------------------


--------------------------------------------------------------------------------
--
---- Dependently typed programming
--
-- Traditional mathematics consists of definitions, theorems, and proofs —
-- for example, defining lists, sum, and map; stating the existence of a
-- function that’s pointwise equal to sum ∘ map f, for any f; constructing
-- a function and then proving that it satisfies the specification using
-- equational reasoning.
--
-- Now we have a new kind of language where it is more convenient to do a
-- new style of mathematics where we write more precise definitions and less
-- theorems and proofs — instead of writing programs and then manually proving
-- them correct, we just write programs that have more precise types and are
-- automatically checked by Agda.  Moreover, Agda helps us to write these
-- more precisely typed programs by computing and providing type information
-- interactively.
--
--------------------------------------------------------------------------------

module SimplyTypedQueues where

  data Maybe (A : Set) : Set where
    nothing :     Maybe A
    just    : A → Maybe A

  _++_ : List A → List A → List A
  []       ++ ys = ys
  (x ∷ xs) ++ ys = x ∷ (xs ++ ys)

--------------------------------------------------------------------------------
--
---- A signature of queues
--
-- A minimal signature of queues can be written as the type
--
--   Σ[ State ∶ Set ] State × (State → A → State) × (State → Maybe (A × State))
--
-- requiring that there exists a state type on which we can perform three
-- operations: constructing an empty state, pushing an element into a state
-- (from the right), and possibly popping an element from a state (from the
-- left).  This type can be written more comprehensibly as a record type:

  record Queue (A : Set) : Set₁ where field
    State  : Set
    nil    : State
    snoc   : State → A → State
    uncons : State → Maybe (A × State)

-- This is merely a signature of queues, not a specification, because little
-- is said about the behaviour of an implementation of this type.  For example,
-- the following implementation using a list as the state is both type-correct
-- and behaviourally correct (although inefficient, which the types don’t talk
-- about),

  listQueue : (A : Set) → Queue A
  listQueue A = record
    { State  = List A
    ; nil    = []
    ; snoc   = λ xs x → xs ++ (x ∷ [])
    ; uncons = λ { []       → nothing
                 ; (x ∷ xs) → just (x , xs) } }

-- whereas this implementation is only type-correct and has wrong behaviour:

  listQueue' : (A : Set) → Queue A
  listQueue' A = record
    { State  = List A
    ; nil    = []
    ; snoc   = λ xs x → x ∷ (x ∷ xs)
    ; uncons = λ _ → nothing }

-- Can we make type correctness and behavioural correctness coincide?
--
--------------------------------------------------------------------------------

module NatIndexedQueues where
  variable P : ℕ → Set; i j k : ℕ

--------------------------------------------------------------------------------
--
---- An indexed specification of queues
--
-- How to define queues’ first-in-first-out behaviour?  One way is to label
-- elements with serial numbers as they are pushed in, and then require that
-- elements with the right serial numbers are popped.  We do so by refining
-- the element type A : Set to a type family P : ℕ → Set, whose index is the
-- serial number, and the State type also to a type family ℕ → ℕ → Set, whose
-- two indices mark the range of the serial numbers of the elements currently
-- stored in the state.

  data UnconsResult (P : ℕ → Set) (S : ℕ → ℕ → Set) : ℕ → ℕ → Set where
    empty :                     UnconsResult P S j j
    pop   : P i → S (suc i) j → UnconsResult P S i j

  record Queue (P : ℕ → Set) : Set₁ where field
    State  : ℕ → ℕ → Set
    nil    : State 0 0
    snoc   : State i j → P j → State i (suc j)
    uncons : State i j → UnconsResult P State i j

-- We can do a bit of experiment: starting from nil, which is indexed by an
-- empty range, we push in elements and extend the range, and then we pop out
-- elements and shrink the range.  The range indices help us to push and pop
-- elements with the right serial numbers: the right index is the number to
-- be assigned to the next pushed element, and the left index is the number
-- of the next popped element.

  module _ (q : Queue P) where

    open Queue q

    experiment : _
    experiment = nil

--
--------------------------------------------------------------------------------


--------------------------------------------------------------------------------
--
---- Implementing the indexed specification
--
-- To reimplement a list queue, we refine lists to be indexed by a range too:

  data SList (P : ℕ → Set) : ℕ → ℕ → Set where
    []  :                           SList P j j
    _∷_ : P i → SList P (suc i) j → SList P i j

-- List append needs to be reimplemented, but note that the program looks
-- exactly the same — only the type is made more precise.  In fact, the type
-- has fully specified the behaviour — we cannot type-check anything other
-- than list append!  With range indexing, we are led by the types to think
-- about ranges of elements and thereby handle the elements correctly.

  _++_ : SList P i j → SList P j k → SList P i k
  []       ++ ys = ys
  (x ∷ xs) ++ ys = x ∷ (xs ++ ys)

-- The same can be said for the (quantified) Queue type: it has fully specified
-- queue behaviour.  We cannot type-check an implementation that’s not a queue!
-- And the list queue reimplementation also looks exactly the same as the simply
-- typed version except that the types are refined to talk about range — again,
-- simple things should be simple to prove.  (For more complex implementations,
-- explicit proofs about range may still be needed, but they should be easier
-- than traditional correctness proofs.)

  listQueue : (P : ℕ → Set) → Queue P
  listQueue P = record
    { State  = SList P
    ; nil    = []
    ; snoc   = λ xs x → xs ++ (x ∷ [])
    ; uncons = λ { []       → empty
                 ; (x ∷ xs) → pop x xs } }

-- But we haven’t proved that the type fully determines the behaviour of its
-- implementations.  If we think sceptically, the types of Queue operations
-- talk only about serial numbers, not really about elements; how do we know
-- that a popped element is the one pushed in with the same serial number, or
-- that a queue cannot pop an element before one with the same number is pushed?
--
--------------------------------------------------------------------------------

data Dec (A : Set) : Set where
  yes :   A → Dec A
  no  : ¬ A → Dec A

record T : Set where

True : Dec A → Set
True (yes _) = T
True (no  _) = ⊥

witness : {d : Dec A} → True d → A
witness {d = yes a} _ = a
witness {d = no  _} ()

variable m n : ℕ

data _<_ : ℕ → ℕ → Set where
  zero : zero < suc n
  suc  : m < n → suc m < suc n

_<?_ : (x y : ℕ) → Dec (x < y)
_     <? zero  = no λ ()
zero  <? suc y = yes zero
suc x <? suc y with x <? y
...            | yes x<y = yes (suc x<y)
...            | no ¬x<y = no λ { (suc x<y) → ¬x<y x<y }

data Vec (A : Set) : ℕ → Set where
  []  :               Vec A zero
  _∷_ : A → Vec A n → Vec A (suc n)

replicate : (n : ℕ) → A → Vec A n
replicate zero    x = []
replicate (suc n) x = x ∷ replicate n x

elem : Vec A n → m < n → A
elem (x ∷ xs) zero    = x
elem (_ ∷ xs) (suc m) = elem xs m

_!!_ : Vec A n → (i : ℕ) → {True (i <? n)} → A
(xs !! i) {t} = elem xs (witness t)

--------------------------------------------------------------------------------
--
---- Parametricity
--
-- The key is that the type Queue P doesn’t necessarily determine behaviour —
-- the *quantified* type (P : ℕ → Set) → Queue P does.  Let’s look at a simple
-- example first:

id : (A : Set) → A → A
id A x = x

-- The only things we know about A are (i) it is a type and (ii) there is an
-- inhabitant x : A, and we cannot, for example, perform pattern matching on A
-- to learn more about it.  Therefore the only way to construct the required
-- output of type A is to use x.  We may use x arbitrarily, for example,

id' : (A : Set) → A → A
id' A x = replicate 42 x !! 7

-- but ultimately the output can only come from the input.
--
-- With *parametricity*, which can be thought of as a specialised induction
-- principle on programs of a given type (which only becomes nontrivial when
-- the type involves quantification over types or type families), we can prove
-- that the output of any program of type (A : Set) → A → A is the same as its
-- (second) input.
--
--------------------------------------------------------------------------------


--------------------------------------------------------------------------------
--
---- A legit queue interface as a type
--
-- In the same spirit as id, an implementation of type (P : ℕ → Set) → Queue P
-- can only pop elements that are previously pushed in. And since every element
-- type P i is pushed and popped at most once and in order, we know that elements
-- must be pushed and popped correctly in queue order.
--
-- Formally, we can use parametricity to prove that all implementations of
-- type (P : ℕ → Set) → Queue P have the same behaviour.  And since we have
-- a reference implementation listQueue, we know what that behaviour is.
-- The entire proof has been formalised in Agda and described in my blog post:
--
--   Specifying queue order using parametric types, abstractly
--   https://josh-hs-ko.github.io/blog/0048/
--
-- Notably, the proof works for all type-correct implementations, once and
-- for all, and doesn’t need to look into the implementations.
--
-- To sum up:  We have reduced behavioural correctness of queues to type
-- correctness, which can be checked mechanically!  Moreover, the reduction
-- is also formalised in Agda (with its correctness checked mechanically).
-- The quantified Queue type is thus a legit queue interface with behavioural
-- guarantees, not just an approximation (signature) in traditional languages.
-- Such is the unprecedented expressive power bestowed upon —and to be better
-- harnessed by— the dependently typed programmer.
--
--------------------------------------------------------------------------------

variable
  I : Set
  R : I → I → Set
  i j k : I

--------------------------------------------------------------------------------
--
---- Parametric indexing and deques
--
-- In my blog post I quantify over the index type too:

data UnconsResult {I : Set} (R S : I → I → Set) : I → I → Set where
  empty :                 UnconsResult R S k k
  pop   : R i j → S j k → UnconsResult R S i k

record Queue {I : Set} (i₀ : I) (R : I → I → Set) : Set₁ where field
  State  : I → I → Set
  nil    : State i₀ i₀
  snoc   : State i j → R j k → State i k
  uncons : State i j → UnconsResult R State i j

-- So the indices can be specialised away, and there is no need to actually
-- count.  Moreover, this specification can then be directly expanded to one
-- for deques, which has many more interesting implementations to explore.

data UnsnocResult {I : Set} (R S : I → I → Set) : I → I → Set where
  empty :                 UnsnocResult R S i i
  pop   : S i j → R j k → UnsnocResult R S i k

record Deque {I : Set} (i₀ : I) (R : I → I → Set) : Set₁ where field
  State  : I → I → Set
  nil    : State i₀ i₀
  cons   : R i j → State j k → State i k
  snoc   : State i j → R j k → State i k
  uncons : State i j → UnconsResult R State i j
  unsnoc : State i j → UnsnocResult R State i j

-- More generally, see the fascinating (but, sadly, currently stagnating)
-- research area of purely functional data structures:
--
--   Chris Okasaki [1999].  Purely Functional Data Structures.
--   Cambridge University Press.  https://doi.org/10.1017/CBO9780511530104
--
--------------------------------------------------------------------------------
