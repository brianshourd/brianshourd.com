---
title: Thing I Learned Today: Functors in Haskell
author: Brian Shourd
date: October 26, 2012
tags: coding, haskell, math
math: true
---

I've been reading through the absolutely fantastic [Learn You a
Haskell](http://www.learnyouahaskell.com) book, and today I read the
[section on
functors](http://learnyouahaskell.com/functors-applicative-functors-and-monoids)
in Haskell. I was a bit puzzled, at first, because I already have a
category-theoretic (and very math-based) [view of what a functor
is](https://en.wikipedia.org/wiki/Functor). It took me a bit to see how
these two ideas line up, but I figured it out and thought that I would
present it here.

The Math Perspective
--------------------

Let's start with the math perspective. For the sake of clarity, let's
use "functor" for math-type functors and `Functor` for Haskell-type
functors (we'll see that they are the same, but until then we'll need a
distinction).

In order to understand what a functor is, we need to know what a
*category* is. A category $\mathcal{C}$ contains the following
data:

1.  A collection of objects (not necessarily a set), sometimes called
    $\textrm{Ob} (\mathcal{C})$.
2.  For every pair of objects $A$ and $B$ a collection
    $\textrm{Mor} (A,B)$ called the morphisms from $A$
    to $B$. If $f$ is a morphism from $A$ to
    $B$, we write $f \, : \, A \rightarrow B$ (for
    this reason, morphisms are sometimes called *arrows* in category
    theory).
3.  A concept of composition of morphisms. That is, given a morphism
    $ f \, : \, A \rightarrow B$ and a morphism $g \, : \, B
    \rightarrow C$ there is a morphism $g \circ f \, : \, A
    \rightarrow C$.

It must also satisfy the following axioms:

1.  For every object $A$ there is an element $\textrm{id}_A
    \, : \, A \rightarrow A$ (called the identity morphism of A) such
    that for every morphism $f \, : \, A \rightarrow B$, $\textrm{id}_A
    \circ f = f = f \circ \textrm{id}_A$.
2.  If $f$, $g$, and $h$ are morphisms such that $f \circ (g \circ h)$
    makes sense, then $f \circ (g \circ h) = (f \circ g) \circ h$. That
    is, composition of morphisms is associative.

This is a lot of technical words to encode some really basic ideas. A
lot of the time, the objects of a category are just sets (with algebraic
structure) and morphisms are just functions between sets (that preserve
the algebraic structure). Some classical categories include

1.  $\textrm{Set}$ the category of sets. The objects are all sets,
    and the morphisms are just set-theoretic functions between sets. The
    notion of composition is just that - regular old composition of
    functions.
2.  $\mathbb{C}\textrm{-Vect}$ the category of complex vector
    spaces. The objects are all complex vector spaces (which are just
    [sets with some algebraic
    structure](http://en.wikipedia.org/wiki/Vector_space#Definition) ) and
    the morphisms are just linear maps between vector spaces (which are
    precisely the functions that preserve the algebraic structure) with
    usual composition.
3.  We can also do funny things, like consider a group $G$ to be a
    category containing only one object. Then $\textrm{Mor} (G,G)$
    consists of the elements of $G$, and composition of morphisms is
    given by multiplication of elements of $G$.  This is a great
    example of how morphisms aren't necessarily functions, they are just
    things that can be combined in an associative way (of which functions
    are a particular example).

Now that we have a category, we can define a functor. Given categories
$\mathcal{C}$ and $\mathcal{D}$, a functor $F \, : \,
\mathcal{C} \rightarrow \mathcal{D}$ is a rule that assigns to each
object $A$ in $\mathcal{C}$ an object $F (A)$ in
$\mathcal{D}$. It also assigns to every morphism $f \, : \,
A \rightarrow B$ of objects of $\mathcal{C}$ a morphism $F
(f) \, : \, F(A) \rightarrow F(B)$, and satisfies the following axioms:

1.  For every object $A$ in $\mathcal{C}$, $F(\textrm{id}_A) =
    \textrm{id}_{F(A)}$. That is, $F$ preserves
    identity morphisms.
2.  If $f \, : \, A \rightarrow B$ and $g \, : \, B
    \rightarrow C$ are morphisms of objects in $\mathcal{C}$, then
    $F(g \circ f) = F(g) \circ F(f)$. That is, $F$ respects
    composition of morphisms.

> Note: technically speaking, what we've defined is a *covariant*
> functor. There are also functors which reverse the order of maps, so
> that if $f \, : \, A \rightarrow B$, then $F(f) \, : \, B
> \rightarrow A$, which we call *contravariant*. Covariant functors are
> often just called functors, and the covariant is omitted unless we
> want to explicitely say that it is not contravariant.

The Haskell Perspective
-----------------------

In Haskell, a `Functor` is a special version of a type constructor. A
type constructor is a structure that takes a concrete type and creates a
new concrete type. For example, `Maybe` is a type constructor because if
we give it the concrete type `Int` we get a new concrete type
`Maybe Int`. So is `[]`, which takes a type like `Char` and gives us a
type `[Char]`.

In order for `Functor` to match up with the math version of functors,
each functor must be take objects of some category $\mathcal{C}$
to objects of some category $\mathcal{D}$. It must be that both of
these categories are just the category of all concrete types. We can
check that this is, in fact, a category, with morphisms just functions
between types. We have the identity morphism (called `id` in Haskell),
and we also have associativity of composition.

What else do we need? Well, for one thing, we need a `Functor` to act on
morphisms, not just objects. That is, if we have a type constructor
`Fun`, two types `a` and `b`, and a function `f ::  a -> b`, we expect
that there is somehow a function `f' :: Fun a -> Fun b`. Indeed, there
is! In order to make `Fun` a `Functor`, we use the code

~~~{.haskell}
instance Functor Fun where
    fmap :: (a -> b) -> Fun a -> Fun b
~~~

So we have to actually define a function `fmap` which takes functions
`f :: a -> b` to functions `fmap f :: Fun a -> Fun b` in order to make
`Fun` an instance of `Functor`. It's explicitely required by the
language.

But that isn't all - we also had two axioms that functors should
satisfy: they need to preserve the identity and they need to respect
composition of functions. Does Haskell check this too?

It turns out that no, Haskell doesn't. Which isn't surprising, given
that these would be very difficult things for a compiler to check. Sure,
it could try to generate some test cases and run them against the code
as it compiles, but that isn't sufficient (and still seems hard).
Instead, Haskell passes the burden to you, the programmer. If you make
something an instance of `Functor`, you'd better make sure that it
satisfies those axioms. Otherwise, it won't behave right. It won't
behave *functorially* (that's a real word), and all the basic properties
of functors (and `Functors`) that you expect might not work.

These kinds of things make me so excited about learning Haskell. So much
of it is just math stuff that I already know, but changed around, given
different context. A different perspective. It's beautiful, and it helps
me both to understand Haskell, and to better understand the math. Win,
win.
