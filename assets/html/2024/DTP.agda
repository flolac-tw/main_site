--------------------------------------------------------------------------------
--
----  Logic IV: A taste of dependently typed programming in Agda
--
----  Josh Ko (Institute of Information Science, Academia Sinica, Taiwan)
--
--------------------------------------------------------------------------------

variable A B C : Set

--------------------------------------------------------------------------------
--
----  Introducing propositional connectives in Agda
--
--  The NJ introduction rules of connectives define the meaning of the
--  connectives by specifying the canonical proofs of the connectives.
--  They correspond to the types of the constructors in datatype definitions
--  in Agda.
--
--  Conjunction corresponds to product/pair types:

data _×_ (A B : Set) : Set where
  _,_ : A → B → A × B

infix 3 _×_

--  Disjunction corresponds to binary sum types:

data _⊎_ (A B : Set) : Set where
  inl : A → A ⊎ B
  inr : B → A ⊎ B

infix 3 _⊎_

--  Implication corresponds to function types, which are primitive in Agda and
--  do not require a definition.
--
--  Falsity corresponds to an empty datatype that has no constructors:

data ⊥ : Set where

¬_ : Set → Set
¬ A = A → ⊥

--  For convenience, we can model truth as a datatype with one constructor:

data ⊤ : Set where
  tt : ⊤

--
--------------------------------------------------------------------------------


--------------------------------------------------------------------------------
--
----  Eliminating propositional connectives in Agda
--
--  The NJ elimination rules can be defined by pattern matching, which is
--  the primitive elimination form in Agda (differing from conventional
--  Type Theory).

outl : A × B → A
outl = {!   !}

outr : A × B → B
outr = {!   !}

case : A ⊎ B → (A → C) → (B → C) → C
case = {!   !}

abort : ⊥ → A
abort = {!   !}

--  We can now re-prove some familiar logical theorems in Agda using the
--  constructors of the logical datatypes (corresponding to the introduction
--  rules) and the eliminators above (corresponding to the elimination rules).

distr : A × (B ⊎ C) → (A × B) ⊎ (A × C)
distr = {!   !}

--  Note the similarity between constructing a program in Agda (interactively)
--  and constructing a derivation in NJ.
--
--------------------------------------------------------------------------------


--------------------------------------------------------------------------------
--
----  Exploiting the full power of Agda
--
--  As long as the important meta-theoretic properties (e.g., consistency and
--  canonicity) still hold, we don’t have to restrict ourselves to the NJ
--  eliminators, and can use more convenient programming constructs offered by
--  Agda (in particular pattern matching).

distr' : A × (B ⊎ C) → (A × B) ⊎ (A × C)
distr' = {!   !}

--
--------------------------------------------------------------------------------


--------------------------------------------------------------------------------
--
----  The double negation monad
--
--  Recall that Glivenko’s theorem tells us that classical reasoning can be
--  conducted in intuitionistic logic under double negation.  Agda, being an
--  intuitionistic language, cannot directly prove classical theorems.  But
--  we can prove that double negation is a monad (automatically!) and conduct
--  classical reasoning within this monad.

return : A → ¬ ¬ A
return = {!   !}

_>>=_ : ¬ ¬ A → (A → ¬ ¬ B) → ¬ ¬ B
_>>=_ = {!   !}

--  The monad laws hold trivially.  (Why?)
--
--  The double negation monad can be thought of as a special kind of Reader
--  that provides classical reasoning principles, such as excluded middle.

LEM : ¬ ¬ (A ⊎ ¬ A)
LEM = {!   !}

--  Within the monad, we can obtain a proof of excluded middle and use it to
--  prove other classical theorems.

DNE : ¬ ¬ (¬ ¬ A → A)
DNE = {!   !}

--
--------------------------------------------------------------------------------


--------------------------------------------------------------------------------
--
---- Quantification and predicates
--
--  As a logic, Agda’s type system is very expressive and goes well beyond
--  propositional logic.  In particular, the type system allows *quantification*
--  over a wide range of entities, allowing us to state and prove more powerful
--  propositions that contain *universal quantification*, like
--
--    ‘a property holds for all x’
--   (‘every x satisfies the property’)
--
--  and *existential quantification*, like
--
--    ‘a property holds for some x’
--   (‘there exists an x that satisfies the property’).
--
--  A ‘property’ above is called a *predicate* in logic.  In Agda, a predicate
--  is represented as a type function P : A → Set, which, to each element x : A,
--  assigns a type P x : Set describing what proof is required for saying that
--  x satisfies the predicate/property.  That is, x satisfies P exactly when
--  we can construct a program of type P x.
--
--  Example:

data ℕ : Set where
  zero :     ℕ
  suc  : ℕ → ℕ

Even : ℕ → Set
Even zero          = ⊤
Even (suc zero)    = ⊥
Even (suc (suc n)) = Even n

Odd : ℕ → Set
Odd zero          = ⊥
Odd (suc zero)    = ⊤
Odd (suc (suc n)) = Odd n

--
--------------------------------------------------------------------------------

variable m n : ℕ

--------------------------------------------------------------------------------
--
----  Universal quantification as dependent function types
--
--  A proof that a predicate P : A → Set holds *for all* x : A is a function
--  that produces a proof of P x upon receiving any x : A.  Note that in the
--  type of the function, the return type P x depends on its input x:
--
--    (x : A) → P x
--
--  This is exactly a dependent function type in Agda.  It is also called
--  a Π-type since a function of the above type can be seen as an element
--  of the product of the A-indexed family of types { P x | x ∈ A }.
--
--  Traditionally a universal quantification is written ‘∀x. P(x)’.
--  (The domain is left implicit at syntax level and, as a part of a semantic
--  interpretation, specified uniformly for all quantifiers in a formula.)
--
--  Example: we can state and prove the proposition ‘every natural number is
--  either even or odd’ as follows:

even-or-odd : (n : ℕ) → Even n ⊎ Odd n
even-or-odd = {!   !}

--  Note that there is some computation going on at type level (which expands
--  the definition of Even and Odd).  More generally speaking, think of Agda’s
--  type checking as being performed on the normal form of types.
--
--  From this example we also see that a proof by induction is just a
--  structurally recursive program in Agda.
--
--------------------------------------------------------------------------------


--------------------------------------------------------------------------------
--
----  Existential quantification as dependent pair types
--
--  A proof that a predicate P : A → Set holds *for some* x : A is a pair whose
--  first component is an element x : A and whose second component is a proof
--  of P x.  Note that the type of the second component depends on (the value of)
--  the first component.  These dependent pair types are defined in Agda as

data Σ (A : Set) (P : A → Set) : Set where
  _,_ : (x : A) → P x → Σ A P

infix 2 Σ

--  on which we can define two eliminators:

proj₁ : {P : A → Set} → Σ A P → A
proj₁ (a , p) = a

proj₂ : {P : A → Set} → (s : Σ A P) → P (proj₁ s)
proj₂ (a , p) = p

--  We can make the syntax more natural in Agda with the following declaration:

syntax Σ A (λ x → M) = Σ[ x ∶ A ] M

--  Dependent pair types are named Σ-types because a dependent pair can be seen
--  as an element of the sum of the A-indexed family of types { P x | x ∈ A }.
--
--  Traditionally an existential quantification is written ‘∃x. P(x)’.
--
--  Example: we can state and prove the proposition ‘there exists an even
--  natural number’ as follows:

exists-even : Σ[ n ∶ ℕ ] Even n
exists-even = {!   !}

--  Σ-types are a *constructive* version of existential quantification: to prove
--  Σ[ x ∶ A ] P x, we must construct a witness x and prove that it satisfies P
--  rather than prove ¬((x : A) → ¬ P x), which is permitted in classical logic.
--
--------------------------------------------------------------------------------


--------------------------------------------------------------------------------
--
----  Constructivity is the default
--
--  Constructively, a proof of a ∀-∃ theorem is also an algorithm satisfying a
--  specification.

data _≤_ : ℕ → ℕ → Set where
  base : zero ≤ m
  step : m ≤ n → suc m ≤ suc n

findEven : (m : ℕ) → Σ[ n ∶ ℕ ] (m ≤ n) × Even n
findEven = {!   !}

--  We can extract the algorithm simply by projection because Σ is constructive.

findEven' : Σ[ f ∶ (ℕ → ℕ) ] ((m : ℕ) → (m ≤ f m) × Even (f m))
findEven' = {!   !}

--  There is no need to say informally whether a proof is constructive or not:
--  constructivity is the default.  A non-constructive proof would be clearly
--  indicated in the theorem statement (through a negative translation or
--  truncation in HoTT).  This is similar with Haskell and monadic programming:
--  purity is the default, and the use of effects would be clearly indicated in
--  monadic types.
--
--------------------------------------------------------------------------------


--------------------------------------------------------------------------------
--
----  The Curry–Howard distinguisher
--
--  To determine whether someone thinks about predicate logic in the Curry–Howard
--  way, ask them why the following two propositions are equivalent:
--
--    ∀x. (P(x) → Q)    and    (∃x. P(x)) → Q
--
--  To the Curry–Howard eye, the two propositions are immediately recognised as
--  a curried function type and its uncurried version.

uncurry : {P : A → Set} {Q : Set}
        → ((x : A) → P x → Q) → ((Σ[ x ∶ A ] P x) → Q)
uncurry = {!   !}

curry : {P : A → Set} {Q : Set}
      → ((Σ[ x ∶ A ] P x) → Q) → ((x : A) → P x → Q)
curry = {!   !}

--
--------------------------------------------------------------------------------


--------------------------------------------------------------------------------
--
----  Equality types
--
--  The equality predicate _≡_ on two elements of type A is defined such that
--  x ≡ y is inhabited (by refl) exactly when x and y have the same normal form.

data _≡_ {A : Set} : A → A → Set where
  refl : {x : A} → x ≡ x

infix 2 _≡_

--  For example, defining addition of natural numbers as

_+_ : ℕ → ℕ → ℕ
zero  + n = n
suc m + n = suc (m + n)

infixr 5 _+_

--  Agda can directly confirm that 1 + 1 ≡ 2:

principia : suc zero + suc zero ≡ suc (suc zero)
principia = {!   !}

--  On the other hand, the type 1 + 1 ≡ 3 is uninhabited.  In fact we can prove
--  its negation by matching any given proof of the type against the empty
--  pattern ‘()’, which instructs Agda to check that it’s impossible for the
--  type to be inhabited (since zero and suc are never equal):

1+1≢3 : ¬ (suc zero + suc zero ≡ suc (suc (suc zero)))
1+1≢3 = {!   !}

--
--------------------------------------------------------------------------------

cong : (f : A → B) {x y : A} → x ≡ y → f x ≡ f y
cong f refl = refl

--------------------------------------------------------------------------------
--
----  Equational reasoning about lists
--
--  Given the following definitions about lists,

data List (A : Set) : Set where
  []  :              List A
  _∷_ : A → List A → List A

_++_ : List A → List A → List A
[]       ++ ys = ys
(x ∷ xs) ++ ys = x ∷ (xs ++ ys)

length : List A → ℕ
length []       = zero
length (x ∷ xs) = suc (length xs)

--  we can prove the following equation:

++-length : (xs ys : List A) → length (xs ++ ys) ≡ length xs + length ys
++-length = {!   !}

--
--------------------------------------------------------------------------------


--------------------------------------------------------------------------------
--
----  Equational reasoning combinators
--
--  To make equational proofs human-readable, we can use the following cleverly
--  designed combinators:

begin_ : {x y : A} → x ≡ y → x ≡ y
begin eq = eq

_≡⟨_⟩_ : (x {y z} : A) → x ≡ y → y ≡ z → x ≡ z
x ≡⟨ refl ⟩ y≡z = y≡z

_∎ : (x : A) → x ≡ x
x ∎ = refl

infix  1 begin_
infixr 2 _≡⟨_⟩_
infix  3 _∎

--  With the combinators, we can write equational proofs in a style that is much
--  closer to what we write on paper; moreover, Agda checks the validity of the
--  proofs for us!

++-length' : (xs ys : List A) → length (xs ++ ys) ≡ length xs + length ys
++-length' = {!   !}

--
--------------------------------------------------------------------------------


--------------------------------------------------------------------------------
--
----  Vectors: length-indexed lists
--
--  The distinguishing feature of dependently typed programming is the use of
--  *indexed datatypes* (or *inductive families*) to encode inductive properties
--  and proofs.  The best known example is the vector datatype: Vec A : ℕ → Set
--  is a family of types, each of which is inhabited by lists (with elements of
--  type A) of a particular length.

data Vec (A : Set) : ℕ → Set where
  []  :               Vec A  zero
  _∷_ : A → Vec A n → Vec A (suc n)

--  Length is now a property encoded *intrinsically* in the definition of
--  vectors, and we can state length-related properties directly using the
--  length index.

_++ⱽ_ : Vec A m → Vec A n → Vec A (m + n)
xs ++ⱽ ys = {!   !}

--  Note that there is no longer need for a separate proof.  As vectors are
--  constructed and deconstructed, the type checker automatically keeps track of
--  their lengths and checks that all the lengths are as specified in the types.
--  In effect, we have fused the list append program and the proof about list
--  append and length into the vector append program, finishing both in one go.
--
--------------------------------------------------------------------------------


--------------------------------------------------------------------------------
--
----  Datatypes with intrinsic invariants
--
--  Intrinsic typing helps to eliminate inductive proofs for some other examples
--  too.  Another typical example is red-black trees:

data Colour : Set where
  red   : Colour
  black : Colour

data RBTree (A : Set) : Colour → ℕ → Set where
  leaf      :                    RBTree A black zero
  redNode   : A
            → RBTree A black n
            → RBTree A black n → RBTree A red   n
  blackNode : A
            → {c₀ c₁ : Colour}
            → RBTree A c₀ n
            → RBTree A c₁ n    → RBTree A black (suc n)

--  This datatype encodes the red and black properties and is inhabited only
--  by balanced trees.  (We could encode the search tree property as well.)
--  Subsequently, operations implemented on this datatype are all guaranteed
--  to preserve balance as soon as they are type-checked.
--
--  Yet another typical example is intrinsically typed λ-terms, as covered, for
--  example, by the online textbook Programming Language Foundations in Agda
--  (https://plfa.github.io/DeBruijn/).
--
--  For a less typical example, I have a blog post describing the development
--  of a tiny compiler that is intrinsically verified to be semantics-preserving
--  (https://josh-hs-ko.github.io/blog/0010/).  Recently the technique is also
--  used to construct a verified compiler from λ-calculus to combinatory logic
--  (https://doi.org/10.1017/s0956796823000084).
--
--  I’ve also written several papers on intrinsically typed developments of
--  algorithms and data structures (which are all available on my website —
--  ask me for recommendations).
--
--------------------------------------------------------------------------------


--------------------------------------------------------------------------------
--
----  Reflections on intrinsic typing
--
--  Traditional mathematics consists of definitions, theorems, and proofs
--  (defining lists, length, and append; stating the relationship between
--  append and length; proving the relationship with equational reasoning),
--  whereas intrinsically typed programming ‘does it all’ with definitions
--  (defining vectors and vector append).  How?
--
--  The key to the success of intrinsic typing is that the definitions are
--  made so precise that only semantically correct expressions are well formed
--  (type-correct).  On the other hand, it becomes non-trivial to check whether
--  an expression involving intrinsic types is well formed — for example, in
--  the application ‘f xs’ where
--
--    f : Vec A (suc n) → ...    and    xs : Vec A ‘some complex expression’
--
--  we need to reduce the complex expression to determine whether the application
--  is valid.  On paper, the reduction has to be done by hand anyway, so there’s
--  not much advantage using intrinsically typed definitions.  These definitions
--  are nice to use in Agda because Agda can check well-formedness automatically,
--  taking some non-trivial workload away from us.
--
--  Thanks to a new kind of language, it is more convenient to do a new style of
--  mathematics where we write more precise definitions and less theorems and proofs.
--
--------------------------------------------------------------------------------
