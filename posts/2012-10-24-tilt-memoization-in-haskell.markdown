---
title: Thing I Learned Today: Memoization in Haskell
author: Brian Shourd
date: October 24, 2012
tags: coding, haskell
---

Often times, when writing a program, I'll need to call a function many times with the same argument. Sometimes directly, sometimes indirectly. In an imperative language, this function might have side-effects, so calling it multiple times with the same arguments might have different results.

~~~{.python}
#python code
state = 0
def swap_state():
    if state == 0:
        state = 1
    else:
        state = 0
~~~

But in Haskell, most of our functions our pure, so they have no side-effects. No matter when or how we call them, if we use the same parameters we'll get the same result.

~~~{.haskell}
-- haskell code of the [Collatz Conjecture](http://en.wikipedia.org/wiki/Collatz_conjecture)
coll 1 = [1]
coll x = 1 + (coll (next x))
    where
        next y | even y = y `div` 2
               | odd y = 3*y + 1
~~~

Here, `coll 6` always gives us 9. If we then call `coll 12`, we get `1 + coll 6`, which is 10.

Notice that when we call `coll 12`, internally Haskell will replace this with `1 + coll 6`. But we just calculated `coll 6`! Do we really need to do this every time? If I'm trying to find the smallest number with a Collatz cycle bigger than 900, I want it to be speedy!

That's where **memoization** comes in. [Memoization](http://en.wikipedia.org/wiki/Memoization) is the act of storing the value associated to a pure function for later reference. Then the next time we call the same function with the same parameter, we simply look up the result in our storage structure (whatever that may be), because that's faster than recalculating it. We can use any storage structure we want, really - whatever is convenient for the problem at hand.

Implementing this in Haskell is a bit tricky, though. Laziness and other oddities of the language (all of which make it powerful, but a bit mind-bending) can muddle things. What I think is that anything declared without parameters will be stored after it is calculated - but the calculations will be done only when they are needed. Anything with parameters will be recalculated every time it is called.

I tried to follow the ideas in [this post](http://stackoverflow.com/questions/3208258/memoization-in-haskell) to memoize our `coll` function, but it didn't actually work. I don't know why yet - perhaps I'll learn that another day! Nevertheless, here's  what I came up with:

First, we'll make coll work a bit differently, so that instead of calling itself again, it calls a supplied function.

~~~{.haskell}
coll' :: (Int -&gt; Int) -&gt; Int -&gt; Int
coll' f x = 1 + (f (next x))
    where
        next y | even y = y `div` 2
               | odd y = 3*y + 1
~~~

*Side Note: If we call `fix coll'` we get `coll` back. I'll talk about this another day.*

Now we define a helper function and a list to store the results.

~~~{.haskell}
collList = map (coll' fast_col) [1..]

fast_col = (collList !!) . (subtract 1)
~~~

But `fast_col` is **not** faster on repeat lookups, so this doesn't actually seem to be memoizing the values. I'll just have to keep learning, I guess.
