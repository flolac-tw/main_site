--------------------------------------------------------------------------------
--
----  Dependently Typed Programming: Part 3
--
----  An Intrinsically Typed Compiler
--
----  Josh Ko (Institute of Information Science, Academia Sinica)
--
--------------------------------------------------------------------------------


variable A B C : Set

_∘_ : (B → C) → (A → B) → (A → C)
(g ∘ f) x = g (f x)

data _×_ (A B : Set) : Set where
  _,_ : A → B → A × B

data Unit : Set where
  tt : Unit

open import Agda.Builtin.Nat renaming (Nat to ℕ) using (zero; suc; _+_)

variable k m n : ℕ

data List (A : Set) : Set where
  []  : List A
  _∷_ : A → List A → List A

infixr 8 _∷_

open import Agda.Builtin.Equality using (_≡_; refl)

cong : (f : A → B) {x y : A} → x ≡ y → f x ≡ f y
cong f refl = refl

begin_ : {x y : A} → x ≡ y → x ≡ y
begin eq = eq

_≡⟨_⟩_ : (x {y z} : A) → x ≡ y → y ≡ z → x ≡ z
x ≡⟨ refl ⟩ y≡z = y≡z

_∎ : (x : A) → x ≡ x
x ∎ = refl

infix  1 begin_
infixr 2 _≡⟨_⟩_
infix  3 _∎


--------------------------------------------------------------------------------
--
----  Extrinsic types
--
--  Write a (simply typed) program, and then state and prove its properties
--  separately.
--
--  The typical example is list append and how it interacts with length:

_++_ : List A → List A → List A
[]       ++ ys = ys
(x ∷ xs) ++ ys = x ∷ (xs ++ ys)

length : List A → ℕ
length []       = 0
length (x ∷ xs) = 1 + length xs

++-length : (xs ys : List A) → length (xs ++ ys) ≡ length xs + length ys
++-length []       ys = refl
++-length (x ∷ xs) ys =
  begin
    length ((x ∷ xs) ++ ys)
  ≡⟨ refl ⟩
    length (x ∷ (xs ++ ys))
  ≡⟨ refl ⟩
    1 + length (xs ++ ys)
  ≡⟨ cong (1 +_) (++-length xs ys) ⟩
    1 + length xs + length ys
  ≡⟨ refl ⟩
    length (x ∷ xs) + length ys
  ∎

--
--------------------------------------------------------------------------------


--------------------------------------------------------------------------------
--
----  Intrinsic types
--
--  We could have convinced ourselves of the truth of ++-length just by going
--  through the definition of _++_ and tracing the length of the lists appearing
--  in the definition.
--
--  In fact, the dependent type system can do the tracing for us: just work with
--  vectors instead of lists.

data Vec (A : Set) : ℕ → Set where
  []  : Vec A 0
  _∷_ : A → Vec A n → Vec A (1 + n)

_++ⱽ_ : Vec A m → Vec A n → Vec A (m + n)
[]       ++ⱽ ys = ys
(x ∷ xs) ++ⱽ ys = x ∷ (xs ++ⱽ ys)

--  What we will see next pushes the idea of intrinsically typed programming
--  to an extreme and formalises some of the underlying principles.
--
--  * Conor McBride [2011]. Ornamental algebras, algebraic ornaments.
--    https://personal.cis.strath.ac.uk/conor.mcbride/pub/OAAO/LitOrn.pdf.
--
--------------------------------------------------------------------------------


head : Vec A (1 + n) → A
head (x ∷ xs) = x


--------------------------------------------------------------------------------
--
----  Hutton’s razor
--  
--  We start with a simple language of additive expressions and its evaluation.

data Expr : Set where
  lit   : ℕ → Expr
  _∔_   : Expr → Expr → Expr

infixl 8 _∔_

eval : Expr → ℕ
eval (lit x) = x
eval (l ∔ r) = eval l + eval r

--
--------------------------------------------------------------------------------


--------------------------------------------------------------------------------
--
----  Eliminating (non-trivial) recursion
--
--  Computing eval is non-trivial (e.g., for an elementary school kid).

testExpr : Expr
testExpr = ((lit 0 ∔ lit 1) ∔ lit 2) ∔ (lit 3 ∔ lit 4)

--  Let us transform the evaluation of an expression into a series of simple
--  instructions (that can be followed by a (clever) elementary school kid).
--
--  One series of instructions that evaluates testExpr on a stack machine:
--
--    push 0 ▷ push 1 ▷ push 2 ▷ add ▷ add ▷ push 3 ▷ push 4 ▷ add ▷ add
--
--------------------------------------------------------------------------------


--------------------------------------------------------------------------------
--
----  Modelling the stack machine (and its instructions)
--
--  There are three kinds of instruction for manipulating a stack:

module M where

  data Prog : Set where

--  we can push a number onto a stack

    push : ℕ → Prog

--  or replace the top two numbers of a stack with their sum,

    add  : Prog

--  and we also need a sequential composition to form a series of instructions.

    _▷_  : Prog → Prog → Prog

--  Formally, representing stacks as lists,

  Stack : Set
  Stack = List ℕ

--  we can define the semantics of the programs ...

  ⟦_⟧ : Prog → Stack → Stack
  ⟦ p ⟧ xs = {!!}

--  ... or can we?
--
--------------------------------------------------------------------------------


--------------------------------------------------------------------------------
--
----  Modelling the stack machine (second attempt)
--
--  The above ⟦_⟧ function is not total since there is no guarantee that ⟦ add ⟧
--  only operates on lists with at least two elements.
--
--  Instead of coping with instructions that can fail, we can work with only
--  instructions that operate on stacks with enough elements and thus are
--  guaranteed to succeed.
--
--  We put two indices into Prog stating the stack heights before and after
--  a program is executed (a lightweight form of pre- and post-conditions):

data Prog : ℕ → ℕ → Set where
  push : ℕ → Prog n (1 + n)
  add  : Prog (2 + n) (1 + n)
  _▷_  : Prog k m → Prog m n → Prog k n

infixr 5 _▷_

--  Redefining stacks as vectors,

Stack : ℕ → Set
Stack = Vec ℕ

--  we can now redefine ⟦_⟧ to operate on stacks of specific heights only:

⟦_⟧ : Prog m n → Stack m → Stack n
⟦ p ⟧ xs = {!!}

--
--------------------------------------------------------------------------------


--------------------------------------------------------------------------------
--
----  Compiling an expression to its evaluation on the stack machine
--
--  The compiler turns an expression into a program that increases the stack
--  height by one:

compile : Expr → Prog n (1 + n)
compile e = {!!}

--  More specifically, if we run a program compiled from an expression e on an
--  empty stack, we will end up with eval e at the top of the stack after the
--  program is executed.

soundness : (e : Expr) → head (⟦ compile e ⟧ []) ≡ eval e
soundness e = {!!}

--  This is a kind of soundness property because we are saying that a syntactic
--  transformation has the intended semantic effect.
--
--------------------------------------------------------------------------------


--------------------------------------------------------------------------------
--
----  Switching to intrinsic types
--
--  The key observation here is that the soundness argument can be carried out
--  in essentially the same way as ++-length, because we can explain what a
--  series of compiled instructions does by going through the definition of
--  compile.
--
--  Here the property we trace is more involved than length: it’s the semantics
--  of the instructions.  We therefore index Prog with its semantics:

apply : (ℕ → ℕ → ℕ) → Stack (2 + n) → Stack (1 + n)
apply f (x ∷ y ∷ xs) = f x y ∷ xs

data Prog⁺ : (m n : ℕ) → (Stack m → Stack n) → Set where
  push  : (x : ℕ) → Prog⁺ n (1 + n) (x ∷_)
  add   : Prog⁺ (2 + n) (1 + n) (apply _+_)
  _▷_   : ∀ {f g} → Prog⁺ k m f → Prog⁺ m n g → Prog⁺ k n (g ∘ f)

--  Now we can state the semantics of a compiled program in the type of compile:

compile⁺ : (e : Expr) → Prog⁺ n (1 + n) (eval e ∷_)
compile⁺ e = {!!}

--  There is no need to write a soundness proof for compile anymore!
--
--------------------------------------------------------------------------------


--------------------------------------------------------------------------------
--
----  Exercises
--
--  0. How does the type-checking of compile⁺ go through?
--
--  1. Extend the Expr language (and all the subsequent definitions) with
--     multiplication (or some other operator of your choice).
--
--  2. Generalising Exercise 1 a bit, we could allow the user to supply whatever
--     binary operator they want to use by extending Expr with the constructor
--
--       binOp : (ℕ → ℕ → ℕ) → Expr → Expr → Expr
--
--     One could argue that the resulting stack machine instruction is no longer
--     sensible, though.  Why?
--
--  3. With Prog and Prog⁺, we haven’t actually eliminated non-trivial recursion
--     — the _▷_ operator still leads to a binary tree structure.  Two possible
--     solutions:
--
--     (i) Define a function
--
--           normalise : Prog⁺ m n f → Prog⁺ m n f
--
--         that rotates a program tree to a right-leaning tree (which is the
--         same as a list).  Note that the type of normalise says that it
--         doesn’t change the semantics of the program.
--
--    (ii) Redefine Prog to have a list-like structure, so that ⟦_⟧ becomes
--         tail-recursive.  Why is tail recursion desirable in this case?
--
--  4. Can you extend Expr with a conditional operator
--
--       ifz : Expr → Expr → Expr → Expr
--
--     with the semantics
--
--       eval (ifz c t e) = if eval c == 0 then eval t else eval e
--
--     and make the intrinsically typed compiler work?  What about the
--     extrinsically typed compiler and its soundness proof?
--
--  5. Can you generalise binOp in Exercise 2 to
--
--       op : (n : ℕ) → (ℕ ^ n → ℕ) → Expr ^ n → Expr
--
--     where
--
--       _^_ : Set → ℕ → Set
--       A ^ zero  = Unit
--       A ^ suc n = A × (A ^ n)
--
--     and make the intrinsically typed compiler work?  What about the
--     extrinsically typed compiler and its soundness proof?
--
--------------------------------------------------------------------------------


--------------------------------------------------------------------------------
--
---- The recomputation lemma
--
--  We obtained Prog⁺ by encoding the computation of ⟦_⟧ into Prog as a new
--  index.  The meaning of the new index can be stated formally by the following
--  lemma, which says that the index of a Prog⁺ element can be recomputed by ⟦_⟧
--  from the underlying Prog element:

forget : ∀ {f} → Prog⁺ m n f → Prog m n
forget (push x)  = push x
forget add       = add
forget (p ▷ q)   = forget p ▷ forget q

recomputation : ∀ {f} (p : Prog⁺ m n f) xs → ⟦ forget p ⟧ xs ≡ f xs
recomputation p xs = {!!}

--  Now if we define

compile' : Expr → Prog n (1 + n)
compile' e = forget (compile⁺ e)

-- the soundness of compile' will become a direct corollary of recomputation:

soundness' : (e : Expr) (xs : Stack n) → ⟦ compile' e ⟧ xs ≡ eval e ∷ xs
soundness' e xs = {!!}

--
--------------------------------------------------------------------------------


--------------------------------------------------------------------------------
--
----  Algebraic ornamentation
--
--  In general, whenever a new index is added to datatype by encoding the
--  computation of a fold (a process called ‘algebraic ornamentation’), there
--  is a recomputation lemma explaining the meaning of the new index.
--
--  We don’t have time to introduce the necessary machinery (datatype-generic
--  programming) for formulating algebraic ornamentation and the recomputation
--  lemma fully generically (quantifying over a range of datatypes), but we can
--  get a taste of the construction by specialising it to lists.

foldr : (A → B → B) → B → List A → B
foldr f e []       = e
foldr f e (x ∷ xs) = f x (foldr f e xs)

data AlgList (A {B} : Set) (f : A → B → B) (e : B) : B → Set where
  []  : AlgList A f e e
  _∷_ : (x : A) {y : B} → AlgList A f e y → AlgList A f e (f x y)

--  Exercises:
--
--   (i) Show that Vec is a special case of AlgList.
--
--  (ii) State and prove the recomputation lemma for AlgList.
--
-- (iii) Use the recomputation lemma for AlgList to prove ++-length.
--
--  (iv) Fold fusion relates two folds, which can be encoded into two instances
--       of AlgList; how are the two instances of AlgList related?
--
--------------------------------------------------------------------------------
