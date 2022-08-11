--------------------------------------------------------------------------------
--
----  Dependently Typed Programming: Part 1
--
----  Shallow Embedding of Higher-Order  Logic and
----  Deep    Embedding of Propositional Logic
--
----  Josh Ko (Institute of Information Science, Academia Sinica, Taiwan)
--
--------------------------------------------------------------------------------

variable A B C : Set

open import Agda.Builtin.Char using (Char)


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
--  Falsity (⊥) corresponds to an empty datatype that has no constructors:

data Empty : Set where

Neg : Set → Set
Neg A = A → Empty

--  For convenience, we can model truth (⊤) as a datatype with one constructor:

data Unit : Set where
  tt : Unit

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
outl (a , b) = a

outr : A × B → B
outr (a , b) = b

case : A ⊎ B → (A → C) → (B → C) → C
case (inl a) f g = f a
case (inr b) f g = g b

abort : Empty → A
abort ()

--  We can now re-prove some familiar logical theorems in Agda using the
--  constructors of the logical datatypes (corresponding to the introduction
--  rules) and the eliminators above (corresponding to the elimination rules).
--
--  Note the similarity between constructing a program in Agda (interactively)
--  and constructing a derivation in NJ.

assoc : (A × B) × C → A × (B × C)
assoc = {!!}

distr : A × (B ⊎ C) → (A × B) ⊎ (A × C)
distr = {!!}

--
--------------------------------------------------------------------------------


--------------------------------------------------------------------------------
--
----  A shallowly embedded domain-specific language
--
--  What we have achieved so far is defining NJ as a *domain-specific language*
--  (DSL) for deducing propositional logic theorems in Agda.
--
--  Moreover, we’ve defined NJ as an *embedded* DSL, meaning that the definition
--  is expressed in a host programming language, in this case Agda.
--
--  And more specifically, the DSL is a ‘shallowly embedded’ one, whose
--  constructs (propositions and inference rules) are not only expressed in
--  terms of Agda constructs (datatypes and functions) but also borrow the
--  semantics of the latter.  This is possible because Agda’s type system itself
--  is an intuitionistic logic.
--
--------------------------------------------------------------------------------


--------------------------------------------------------------------------------
--
---- Exploiting the full power of Agda
--
--  As long as the important meta-theoretic properties (e.g., consistency and
--  canonicity) still hold, we don’t have to restrict ourselves to the NJ
--  eliminators, and can use more convenient constructs offered by Agda (in
--  particular pattern matching).

assoc' : (A × B) × C → A × (B × C)
assoc' = {!!}

distr' : A × (B ⊎ C) → (A × B) ⊎ (A × C)
distr' = {!!}

--  Try to prove some other propositions yourselves (for example, the
--  irrefutability of the law of excluded middle).

lem-irrefutable : Neg (Neg (A ⊎ Neg A))
lem-irrefutable = {!!}

--  Note that there is some computation going on at type level (which expands
--  the definition of Neg).  More generally speaking, think of Agda’s type-
--  checking as being performed on the normal form of types.
--
--------------------------------------------------------------------------------


--------------------------------------------------------------------------------
--
---- The double negation monad
--
--  Double negation gives rise to a monad.
--  Classicality (e.g., LEM) is an effect in intuitionistic logic!

DN : Set → Set
DN A = Neg (Neg A)

return : A → DN A
return x = {!!}

_>>=_ : DN A → (A → DN B) → DN B
ma >>= f = {!!}

ite : DN (((A → B) × (Neg A → C)) → (A × B) ⊎ (Neg A × C))
ite = lem-irrefutable >>= λ a⊎¬a →
      return (case a⊎¬a (λ  a fg → inl ( a , outl fg  a))
                        (λ ¬a fg → inr (¬a , outr fg ¬a)))

--  The double negation monad is a special case of the ‘continuation monad’
--  (look it up or ask Prof Shan about this):

Cont : Set → Set → Set
Cont C A = (A → C) → C

returnᶜ : A → Cont C A
returnᶜ = {!!}

_>>=ᶜ_ : Cont C A → (A → Cont C B) → Cont C B
ma >>=ᶜ f = {!!}

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
  zero : ℕ
  suc  : ℕ → ℕ

Even : ℕ → Set
Even zero          = Unit
Even (suc zero)    = Empty
Even (suc (suc n)) = Even n

Odd : ℕ → Set
Odd zero          = Empty
Odd (suc zero)    = Unit
Odd (suc (suc n)) = Odd n

--  We will concern ourselves with the abstract reasoning about predicates and
--  quantification first, without looking into the definitions of the predicates.
--
--------------------------------------------------------------------------------


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
--  Traditionally a universal quantification is written ‘∀x. P(x)’.  The
--  domain is left implicit at syntax level and, as a part of a semantic
--  interpretation, specified uniformly for all quantifiers in a formula.
--
--  Example: we can state and prove the proposition ‘every natural number
--  is either even or odd’ as follows:

even-or-odd : (n : ℕ) → Even n ⊎ Odd n
even-or-odd zero          = inl tt
even-or-odd (suc zero   ) = inr tt
even-or-odd (suc (suc n)) = even-or-odd n

--  Exercises:

flip : {P : A → B → Set}
     → ((x : A) (y : B) → P x y) → ((y : B) (x : A) → P x y)
flip = {!!}

--  In Type Theory, universal quantification subsumes both conjunction and
--  implication.  Why?
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

syntax Σ A (λ x → M) = Σ[ x ∈ A ] M

--  Dependent pair types are named Σ-types because a dependent pair can be seen
--  as an element of the sum of the A-indexed family of types { P x | x ∈ A }.
--
--  Traditionally an existential quantification is written ‘∃x. P(x)’.
--
--  Example: we can state and prove the proposition ‘there exists an even
--  natural number’ as follows:

exists-even : Σ[ n ∈ ℕ ] Even n
exists-even = suc (suc (suc (suc zero))) , tt

--  Exercises:

Σ-assoc : {P : A → B → Set}
        → (Σ[ xy ∈ A × B ] P (outl xy) (outr xy)) → Σ[ x ∈ A ] Σ[ y ∈ B ] P x y
Σ-assoc = {!!}

--  In Type Theory, existential quantification subsumes disjunction.  Why?
--
--------------------------------------------------------------------------------


--------------------------------------------------------------------------------
--
----  More exercises
--
--  Prove the ‘choice theorem’:

choice : {R : A → B → Set}
       → ((x : A) → Σ[ y ∈ B ] R x y) → Σ[ f ∈ (A → B) ] ((x : A) → R x (f x))
choice = {!!}

--  This is an axiom (whose truth has to be assumed) in set theory, but the
--  constructive meaning of Π and Σ makes it provable in Type Theory.
--
--  Defining bi-implication by

_↔_ : Set → Set → Set
A ↔ B = (A → B) × (B → A)

infixr 2 _↔_

--  prove the following:

Π-distr-× : {P Q : A → Set}
          → ((x : A) → P x × Q x) ↔ (((x : A) → P x) × ((x : A) → Q x))
Π-distr-× = {!!}

Σ-distr-⊎ : {P Q : A → Set}
          → (Σ[ x ∈ A ] P x ⊎ Q x) ↔ ((Σ[ x ∈ A ] P x) ⊎ (Σ[ x ∈ A ] Q x))
Σ-distr-⊎ = {!!}

--  For the following two bi-implications, only one direction is true:
--
--      {P Q : A → Set}
--    → ((x : A) → P x ⊎ Q x) ↔ ((x : A) → P x) ⊎ ((x : A) → Q x)
--
--      {P Q : A → Set}
--    → (Σ[ x ∈ A ] P x × Q x) ↔ (Σ[ x ∈ A ] P x) × (Σ[ x ∈ A ] Q x)
--
--  Prove the true directions.
--
--  For something slightly more challenging, disprove the false directions
--  (by proving its negation).
--
--------------------------------------------------------------------------------


--------------------------------------------------------------------------------
--
----  Propositional equality
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
principia = refl

--  On the other hand, the type 1 + 1 ≡ 3 is uninhabited.  In fact we can prove
--  its negation by matching any given proof of the type against the empty
--  pattern ‘()’, which instructs Agda to see that it’s impossible for the type
--  to be inhabited as the normal forms of 1 + 1 and 3 cannot possibly be equal:

1+1≢3 : Neg (suc zero + suc zero ≡ suc (suc (suc zero)))
1+1≢3 ()

--
--------------------------------------------------------------------------------


--------------------------------------------------------------------------------
--
----  Properties of equality
--
--  The refl constructor (whose name is short for reflexivity) proves that every
--  element is equal to itself.  Another two important properties are symmetry

sym : {x y : A} → x ≡ y → y ≡ x
sym refl = refl

--  and transitivity:

trans : {x y z : A} → x ≡ y → y ≡ z → x ≡ z
trans refl refl = refl

--  We will also frequently need the congruence property, which says that every
--  function maps equal arguments to equal results:

cong : (f : A → B) {x y : A} → x ≡ y → f x ≡ f y
cong f refl = refl

--  Now as exercises we can state and prove some standard properties of natural
--  numbers, for example, the associativity of natural number addition:

+-assoc : (k m n : ℕ) → (k + m) + n ≡ k + (m + n)
+-assoc = {!!}

--  From this example we also see that a proof by induction is just a
--  structurally recursive program in Agda.
--
--  Exercise: given the following definitions about lists,

data List (A : Set) : Set where
  []  : List A
  _∷_ : A → List A → List A

_++_ : List A → List A → List A
[]       ++ ys = ys
(x ∷ xs) ++ ys = x ∷ (xs ++ ys)

length : List A → ℕ
length []       = zero
length (x ∷ xs) = suc (length xs)

--  prove the following equation about list append and length.

++-length : (xs ys : List A) → length (xs ++ ys) ≡ length xs + length ys
++-length = {!!}

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

--  With the combinators we can write equational proofs in a style that is much
--  closer to what we write on paper; moreover, Agda checks the validity of the
--  proofs for us!

+-assoc' : (k m n : ℕ) → (k + m) + n ≡ k + (m + n)
+-assoc' = {!!}

--  Exercise: rewrite your proof of ++-length (in particular the inductive case)
--  using the equational reasoning combinators.

++-length' : (xs ys : List A) → length (xs ++ ys) ≡ length xs + length ys
++-length' = {!!}

--
--------------------------------------------------------------------------------


--------------------------------------------------------------------------------
--
----  Deep embedding of NJ
--
--  To prove meta-theorems like soundness, we need to encode NJ derivations as
--  elements of an indexed datatype in Agda, so that we can quantify over NJ
--  derivations and perform case analysis on them.  This is called a ‘deep
--  embedding’, where the programs of a DSL are represented syntactically in
--  the host language (meta-language) and are amenable to meta-level analysis.
--
--  Below we will replay most of the things we have written semi-formally or
--  informally on the Logic slides, from the propositional formulas to NJ.

PV : Set
PV = Char

data ℙ : Set where
  var : Char → ℙ
  ⊥   : ℙ
  _∧_ : ℙ → ℙ → ℙ
  _∨_ : ℙ → ℙ → ℙ
  _⇒_ : ℙ → ℙ → ℙ

infixr 5 _∧_ _∨_
infixr 4 _⇒_

variable φ ψ θ : ℙ

¬_ : ℙ → ℙ
¬ φ = φ ⇒ ⊥

infixr 6 ¬_

data Context : Set where
  ∅   : Context
  _,_ : Context → ℙ → Context

infixl 3 _,_

variable Γ : Context

data _∋_ : Context → ℙ → Set where
  zero :         (Γ , φ) ∋ φ
  suc  : Γ ∋ φ → (Γ , ψ) ∋ φ

infix 2 _∋_ _⊢_

data _⊢_ : Context → ℙ → Set where

  assum :  Γ ∋ φ
        → -------
           Γ ⊢ φ

  ⊥E    :  Γ ⊢ ⊥
        → -------
           Γ ⊢ φ

  ∧I    :  Γ ⊢ φ
        →  Γ ⊢ ψ
        → -----------
           Γ ⊢ φ ∧ ψ

  ∧EL   :  Γ ⊢ φ ∧ ψ
        → -----------
           Γ ⊢ φ

  ∧ER   :  Γ ⊢ φ ∧ ψ
        → -----------
           Γ ⊢ ψ

  ∨IL   :  Γ ⊢ φ
        → -----------
           Γ ⊢ φ ∨ ψ

  ∨IR   :  Γ ⊢ ψ
        → -----------
           Γ ⊢ φ ∨ ψ

  ∨E    :  Γ     ⊢ φ ∨ ψ
        →  Γ , φ ⊢ θ
        →  Γ , ψ ⊢ θ
        → ---------------
           Γ     ⊢ θ

  ⇒I    :  Γ , φ ⊢ ψ
        → ---------------
           Γ     ⊢ φ ⇒ ψ

  ⇒E    :  Γ ⊢ φ ⇒ ψ
        →  Γ ⊢ φ
        → -----------
           Γ ⊢ ψ

--  Here are some examples of deeply embedded NJ derivations.

assoc'' : ∅ ⊢ (var 'A' ∧ var 'B') ∧ var 'C' ⇒ var 'A' ∧ (var 'B' ∧ var 'C')
assoc'' = {!!}

distr'' : ∅ ⊢ var 'A' ∧ (var 'B' ∨ var 'C')
            ⇒ (var 'A' ∧ var 'B') ∨ (var 'A' ∧ var 'C')
distr'' = {!!}

--  Note that exactly the same definition of NJ (modulo renaming) will be used
--  as the datatype of λ-terms tomorrow!
--
--------------------------------------------------------------------------------


--------------------------------------------------------------------------------
--
----  Classical semantics
--
--  Now we model the classical semantics of propositional logic in Agda,
--  and then prove (intuitionistically/constructively) that NJ is sound
--  with respect to the classical semantics by mapping every NJ derivation
--  to the corresponding semantic consequence.

data Bool : Set where
  true  : Bool
  false : Bool

and : Bool → Bool → Bool
and true  b = b
and false b = false

or : Bool → Bool → Bool
or true  b = true
or false b = b

imp : Bool → Bool → Bool
imp true  b = b
imp false b = true

⟦_⟧ : ℙ → (PV → Bool) → Bool
⟦ var x ⟧ σ = σ x
⟦ ⊥     ⟧ σ = false
⟦ φ ∧ ψ ⟧ σ = and (⟦ φ ⟧ σ) (⟦ ψ ⟧ σ)
⟦ φ ∨ ψ ⟧ σ = or  (⟦ φ ⟧ σ) (⟦ ψ ⟧ σ)
⟦ φ ⇒ ψ ⟧ σ = imp (⟦ φ ⟧ σ) (⟦ ψ ⟧ σ)

_sat_ : (PV → Bool) → Context → Set
σ sat ∅       = Unit
σ sat (Γ , φ) = σ sat Γ × (⟦ φ ⟧ σ ≡ true)

_⊧_ : Context → ℙ → Set
Γ ⊧ φ = (σ : PV → Bool) → σ sat Γ → ⟦ φ ⟧ σ ≡ true

soundness : Γ ⊢ φ → Γ ⊧ φ
soundness = {!!}

--  When proving soundness, the following boolean case analysis may be helpful.

bcase : (b : Bool) → (b ≡ true → A) → (b ≡ false → A) → A
bcase true  f g = f refl
bcase false f g = g refl

--  Now it is easy to prove (formally!) that it is impossible to construct
--  an NJ derivation of ⊢ ⊥.

consistency : Neg (∅ ⊢ ⊥)
consistency d with soundness d (λ _ → false) tt
consistency d | ()

--  Exercise: define NK (with ¬¬E), and then state and prove Glivenko’s theorem.
--
--------------------------------------------------------------------------------


--------------------------------------------------------------------------------
--
----  Aside: the ‘Agda semantics’ of NJ
--
--  The soundness theorem gives a semantics to the deeply embedded NJ language,
--  but we can also give other semantics, for example mapping NJ back into Agda.

⟦_⟧ᴬ : ℙ → (PV → Set) → Set
⟦ var x ⟧ᴬ σ = σ x
⟦ ⊥     ⟧ᴬ σ = Empty
⟦ φ ∧ ψ ⟧ᴬ σ = ⟦ φ ⟧ᴬ σ × ⟦ ψ ⟧ᴬ σ
⟦ φ ∨ ψ ⟧ᴬ σ = ⟦ φ ⟧ᴬ σ ⊎ ⟦ ψ ⟧ᴬ σ
⟦ φ ⇒ ψ ⟧ᴬ σ = ⟦ φ ⟧ᴬ σ → ⟦ ψ ⟧ᴬ σ

--  Exercise: state and prove the resulting soundness theorem.
--
--  Is there a relationship between this soundness theorem and the shallowly
--  embedded definitions of NJ rules?
--
--  * Jeremy Gibbons and Nicolas Wu [2014]. Folding domain-specific languages:
--    deep and shallow embeddings (functional pearl). In International Conference
--    on Functional Programming (ICFP), pages 339–347. ACM.
--    https://doi.org/10.1145/2628136.2628138.
--
--------------------------------------------------------------------------------
