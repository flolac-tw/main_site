--------------------------------------------------------------------------------
--
----  Logic IV: Datatype-generic theories for dependently typed programming
--
----  Josh Ko (Institute of Information Science, Academia Sinica, Taiwan)
--
--------------------------------------------------------------------------------

--  This file imports the ‘Native Datatype-Generic Programming’ library
--  hosted at https://github.com/josh-hs-ko/NDGP.

open import Prelude
open import Data.Vec hiding (length)

open import Generics.Description
open import Generics.Recursion
open import Generics.Reflection

open import Generics.RecursionScheme
open import Generics.Ornament
open import Generics.Ornament.Description
open import Generics.Ornament.Algebraic
open import Generics.Ornament.Algebraic.Isomorphism

--  Since dependently typed programming depends heavily on designing various
--  indexed datatypes, we need to develop powerful theories of datatypes to
--  make sense of what we’re doing, as well as identify and provide reusable
--  constructions.

--  A simplest example is the theorem
--
--    ‘There is an induction principle for any datatype.’
--
--  This theorem can be formalised datatype-generically, and used to generate
--  the induction principle of (roughly speaking) any datatype with the help of
--  Agda’s metaprogramming mechanism.  The generated induction principle, which
--  is a type, comes with a proof/program, and the generation is (intrinsically)
--  proved to produce correct types and programs.

--  For example, we can generate induction principles for natural numbers and
--  lists,

instance

  NatC : Named (quote ℕ) _
  unNamed NatC = genDataC NatD (genDataT NatD ℕ)
    where NatD = genDataD ℕ

  ListC : Named (quote List) _
  unNamed ListC = genDataC ListD (genDataT ListD List)
    where ListD = genDataD List

  ListFin : Finitary (findDataD (quote List))
  ListFin = [] ∷ (tt ∷ refl ∷ []) ∷ []

unquoteDecl indℕ = defineInd (ind-operator (quote ℕ)) indℕ
unquoteDecl indList = defineInd (ind-operator (quote List)) indList

--  and even for a rather sophisticated datatype (‘BT’ below) that can be used
--  to quantify over all sublists of a particular length:

variable
  n k : ℕ
  A   : Set
  P   : A → Set
  x   : A
  xs  : Vec _ _

data BT : (n k : ℕ) → (Vec A k → Set) → Vec A n → Set₁ where
  tipZ : P []                               → BT n        zero   P xs
  tipS : P xs                               → BT (suc k) (suc k) P xs
  bin  : BT n (suc k)        P           xs
       → BT n      k (λ zs → P (x ∷ zs)) xs → BT (suc n) (suc k) P (x ∷ xs)

-- testBT : BT 4 2 P ('a' ∷ 'b' ∷ 'c' ∷ 'd' ∷ [])
-- testBT = bin (bin (tipS {!   !})
--                   (bin (tipS {!   !})
--                        (tipZ {!   !})))
--              (bin (bin (tipS {!   !})
--                        (tipZ {!   !}))
--                    (tipZ {!   !}))

instance

  BTC : Named (quote BT) _
  unNamed BTC = genDataC BTD (genDataT BTD BT)
    where BTD = genDataD BT

unquoteDecl indBT = defineInd (ind-operator (quote BT)) indBT

--  There’s a common pattern shared by BT and the double negation monad (!)
--  called the continuation-passing style (CPS).  CPS has been extensively
--  studied in functional programming, but never discussed in the context of
--  indexed datatypes.  More generally, there’s probably a close relationship
--  between certain indexed datatypes and programs (such as BT and the program
--  that nondeterministically chooses a sublist from a list) — I’m thinking
--  about formalising this relationship and exploring its consequences.

--  Another example of datatype-generic theories is the theory of ‘ornaments’.
--  An ornament is a ‘diff’ between datatypes — for example, the difference
--  between the datatypes of lists and natural numbers is that lists take an
--  element in the inductive case.

instance

  ListO : DataO (findDataD (quote List)) (findDataD (quote ℕ))
  ListO = record
    { level  = λ _ → tt
    ; applyL = λ (ℓ , _) → record
        { param  = λ _ → tt
        ; index  = λ _ _ → tt
        ; applyP = λ _ → ι ∷ ∺ (Δ[ _ ] ρ ι ι) ∷ ∺ [] } }

--  From the ornament, first we can generate the length function.

private
  lengthP : FoldP
  lengthP = forget (quote List) (quote ℕ)

instance
  lengthC = genFoldC lengthP length

--  Then, from the datatype of lists and the length function, we can generate
--  the datatype of vectors, because the indices in the definition of vectors
--  are exactly computed by length.
--
--  In general, the theory allows us to fuse a fold function on a datatype into
--  that datatype and get an indexed variant of the datatype.

private
  VectOD : DataOD (findDataD (quote List))
  VectOD = AlgOD lengthP

instance
  VectO = ⌈ VectOD ⌉ᵈ

unquoteDecl data Vect constructor [] _∷_ =
  defineByDataD ⌊ VectOD ⌋ᵈ Vect (Vect.[] List.∷ Vect._∷_ List.∷ List.[])

--  And then the theory proves in general that there are isomorphisms such as
--
--    Vect A n  ≅  Σ[ xs ∶ List A ] length xs ≡ n
--
--  formalising the idea that vectors are lists fused with length proofs.  (We
--  could go on and think about how to fuse other forms of proof and manufacture
--  more sophisticated indexed datatypes systematically.)

instance
  VectC : Named (quote Vect) _
  unNamed VectC = genDataC ⌊ VectOD ⌋ᵈ (genDataT ⌊ VectOD ⌋ᵈ Vect)

private
  fromVectP : FoldP
  fromVectP = forget (quote Vect) (quote List)

unquoteDecl fromVect = defineFold fromVectP fromVect

instance
  fromVectC = genFoldC fromVectP fromVect

private
  toVectP : IndP
  toVectP = remember (quote Vect)

unquoteDecl toVect = defineInd toVectP toVect

instance
  toVectC = genIndC toVectP toVect

private
  from-toVectP : IndP
  from-toVectP = forget-remember-inv (quote Vect) (quote List) (inl it)

unquoteDecl from-toVect = defineInd from-toVectP from-toVect

private
  to-fromVectP : IndP
  to-fromVectP = remember-forget-inv (quote Vect) (quote List) (inl it)

unquoteDecl to-fromVect = defineInd to-fromVectP to-fromVect

--------------------------------------------------------------------------------
--
----  Conclusions
--
--  Like in logic, we not only use a language (or deduction system) but also
--  prove theorems (that is, formulate general constructions) about it.
--
--  We like programming, and we want to study it using mathematical methods
--  to figure out how to write better programs.
--
--------------------------------------------------------------------------------
