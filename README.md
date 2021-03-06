Modal programming in Haskell
============================

Haskell 2010 extended with higher-rank polymorphism is approximately
System F*ω* and thus roughly corresponds to intuitionistic higher-order
propositional logic via a Curry–Howard isomorphism. GHC’s static
pointers extension, while having been conceived specifically for
distributed programming, makes it possible to turn Haskell into the
Curry–Howard correspondents of typical modal logics. Since the static
pointers extension only provides the general basis for modal
programming, specific features of languages corresponding to concrete
modal logics must be implemented as libraries. The `modal-programming`
package provides support for writing such libraries.


Modal logics and modal programming languages
--------------------------------------------

A propositional modal logic usually extends a non-modal propositional
logic with the following components:

  * Modalities, which turn propositions into propositions

  * Axioms that involve these modalities

  * A rule, called N, which is of the form “if *A* is a theorem, then
    *UA* is a theorem”, where *U* is a dedicated modality

Therefore, we can build a modal programming language by adding the
following constructs to a non-modal programming language:

  * Modalities, which turn types into types

  * Primitive values whose types involve these modalities

  * A new form of expression, **absolute** *e*, that has type 
    *Universal* *τ*, provided that *e* is a closed expression of
    type *τ*, where *Universal* is a dedicated modality

The crucial points in the above lists are the last ones. Let us look at
these in more detail.

In a modal logic, one assumes the existence of different worlds. Truth
of a proposition depends on the choice of a world, and a theorem is a
proposition that is true in every world. Analogously, in a modal
programming language, the set of inhabitants of a type depends on the
choice of a world, and there is a notion of absolute values: values that
exist in every world.

A proposition *A* is a theorem exactly if it is true unconditionally,
that is, if ⊢ *A*. Therefore, rule N can be formulated, “if ⊢ *A*, then
⊢ *UA*”. Analogously, a value is absolute exactly if it arises as the
result of a closed expression, that is, an expression *e* for which
⊢ *e* : *τ* for some type *τ*. Therefore, the typing rule for
**absolute** expressions can be formulated, “if ⊢ *e* : *τ*, then
 ⊢ **absolute** *e* : *Universal* *τ*”.

While a closed expression is an expression that does not contain any
free variables, we can permit the expressions *e* in expressions
**absolute** *e* to mention global variables, because we can always
replace global variables by the right-hand sides of their definitions.
Therefore, we ultimately define an absolute value as the result of an
expression whose free variables are all global.


Encoding of modal logics and modal programming languages
--------------------------------------------------------

The modalities of a modal logic can be encoded in higher-order
propositional logic as predicates on propositions, and the axioms that
involve these modalities can be rephrased using the encodings of these
modalities. Analogously, the modalities of a modal programming language
can be encoded in System F*ω* as types of kind ∗ → ∗ and thus in Haskell
as types of kind `Type -> Type`, and the primitive values whose types
involve these modalities can be encoded as primitive values whose types
involve the encodings of these modalities instead.

Unfortunately, it is impossible to encode rule N in higher-order
propositional logic, and analogously it is impossible to encode
**absolute** expressions in System F*ω*. In Haskell, however, we can
encode **absolute** using the `static` construct that comes with GHC’s
static pointers extension.


The `modal-programming` package
-------------------------------

The `modal-programming` package provides support for implementing
Curry–Howard correspondents of various modal logics. It comprises a
type class of universal modalities as well as various modal analogs of
type classes that reflect constructs from category theory, like
`Functor` and `Monad`.
