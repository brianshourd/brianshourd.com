---
title: A Monad is Just a Monoid in the Category of Endofunctors
author: Brian Shourd
date: February 1, 2013
tags: math, coding, haskell
---

> "A Monad is just a monoid in the category of endofunctors, what's the
> big deal?" 

If you've study haskell long enough, you eventually run across this
quote, [whose lineage is traced in this Stack Overflow
question](http://stackoverflow.com/questions/3870088/a-monad-is-just-a-monoid-in-the-category-of-endofunctors-whats-the-problem).
The idea is that a new person to haskell is confused about monads, and
is met with this snobbish answer. It is a kind of trope - that people
who use haskell are so lost in their category-theoretic world that they
have forgotten how to interact with normal people. It is simultaneously
used to critisize haskell (and those who use it) and by haskellers to
emphasize the power of abstract thought.

[Side Note: I can't rightly go on without saying that my experience as a
haskell newb has been that nobody in the usual places would say such a
thing in earnestness. All the haskell people that I've talked to are
great, helpful individuals who are just excited about all of this stuff,
not fools or snobs who frown on the disenlightened.]

I'd like to discuss the latter.

This one statement (snarkiness removed), succinctly demonstrates some of
the power of abstraction brought on by things like category theory and
common in haskell. It's like a little haiku.

    Monad is opaque;
    Monoid in category
    of endofunctor

Ok, so I'm bad at haiku. What I mean is that its beauty lies in its
compact deconstruction of a complicated pattern to essential bits - each
of which is already understood.

It isn't, of course, a *good* explanation. Not by a long shot. This
entirely fails to explain why a monad is useful, or to give the reader
the slightest hint of intuition relating to monads.

But as a definition, it can't be beat. Assuming the reader already knows
what monoids are, and what endofunctors are, the reader can then
reconstruct what a monad is, including all of the monad laws.

On reflection, I think that this is what draws me to mathematics in
general. Everything is just patterns that we've seen before. If we can
put language and theory to the patterns themselves, rather than to the
specific instance we observe, then we have gained power over entire
classes of problems at once. Doing arithmetic modulo 7? 7 is prime, so
there are no nontrivial subgroups. Can't remember the exact rules for a
group homomorphism? We know that morphisms preserve structure, so it
must preserve the identity, respect inverses, and respect the group
product. A little work proves that we only *need* the last of these, and
we can reconstruct the first two. 

Anyway, I'm preparing a talk about category theory in functional
programming (for mathematicians), so I've got some of this on my mind.
I'm excited to finish it, hopefully I'll have some more insight to
share.
