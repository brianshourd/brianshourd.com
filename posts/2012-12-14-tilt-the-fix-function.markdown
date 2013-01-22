---
title: Thing I Learned Today: The fix Function
author: Brian Shourd
date: December 14, 2012
tags: coding, haskell
---

Haskell has this abosultely [amazing function called
`fix`](http://harold.hotelling.net/gcdfix.lhs). Let's start with the
definition:

~~~{.haskell}
fix :: (a -> a) -> a
fix f = f $ fix f
~~~

We see from the type signature that `fix` takes a function `a -> a`, and
returns a value of type `a`. In fact, it should find a _fixed point_ of
`f`, some value `x` such that `f x = x`. That is, we expect that

~~~{.haskell}
f (fix f) = fix f
~~~

Look familiar? Yeah, that's just the very definition of `fix`.

I _love_ this. It's such a very mathematical way of defining a function - 
saying that the result of the function is exactly the thing having the
property we want, whatever that is.

But how can this possibly work? After all, this seems like an infinite
recursive loop. First, `fix f` is reduced to `f $ fix f`, which is
reduced to `f $ f $ fix f`, and so on infinitely. It never ends, right?

We're saved by laziness. It may not end, but that depends entirely on
the function we choose for `f`, and Haskell won't worry about it until
it actually needs to.

So if we try `fix (+1)`, we get an infinite loop. It just keeps adding
one and adding one forever. But consider the function `\x -> 0`, which
is completely constant. When Haskell evaluates `fix (\x -> 0)`, it
expands it to `(\x -> 0) $ fix (\x -> 0)`, which it then evaluates to
`0`, since the function `\x -> 0` doesn't depend at all on the input. Go
ahead, try it in GHCI.

Great, so now we have a function which sometimes terminates, at least in
trivial cases. But is it actually useful in any way?

I'm not sure if it is useful or not, actually. But there is one _cool_
thing that we can do with it. We can use `fix` to implement recursion.
Let's start with an example, then look at how it works. Consider the
following function.

~~~{.haskell}
helper :: (Int -> Int) -> Int -> Int
helper _ 0 = 1
helper f x = x * f (x-1)
~~~

Because of currying, we can think of `helper` as a function of type
`(Int -> Int) -> (Int -> Int)`. That is, we feed it a function `Int -> Int` and we get back another function `Int -> Int`. This matches the type signature for `fix` - if we call `fix helper`, we should end up with a function `Int -> Int`.

What will this function do? Let's expand the definition, evaluating it
against, say, `3`.

~~~{.haskell}
fix helper 3 = helper (fix helper) 3
             = 3 * (fix helper 2)
             = 3 * (helper (fix helper) 2)
             = 3 * (2 * (fix helper 1))
             = ...
             = 3 * 2 * 1 * (fix helper 0)
             = 3 * 2 * 1 * (helper (fix helper) 0)
             = 3 * 2 * 1 * 1
             = 6
~~~

In fact, we can see from this example that `fix helper x` will just give
us `x!`, so that `fix helper` is actually just the factorial function.
It is precisely the function which, when plugged in to `helper`, gives
us back itself.

I still have no idea in what way this is useful (not that I don't think
that it _is_ useful, just that I am still ignorant of that usefulness),
but I do think that it's cool. 


