---
title: Thing I Learned Today: Tail Recursive Functions
author: Brian Shourd
date: October 11, 2012
tags: coding, haskell
math: true
---

Today I learned about [using accumulating parameters to make functions tail recursive](http://www.haskell.org/haskellwiki/Performance/Accumulating_parameter).

A function is tail recursive if the last thing it does is call itself. So the function `f(x) = f(x-1); f(0) = 0` is tail recursive, but the function `g(x) = g(x-1) + x; g(0) = 0` is not. When evaluating the function `g`, the computer must act something like:

    g(x): x -> x-1 -> g(x-1) (hold that thought while I do this....ok I'm back) -> g(x-1) + x

The whole time that it is evaluating `g(x-1)`, it has to remember what it still needs to do when it gets back with the result. This compounds, since `g(x-1)` will require calculating `g(x-2)`, the whole time remembering that it will still need to add `x-1` when it gets back from calculating `g(x-2)`. The result is that a whole lot of function calls are sitting on the stack, filling up ram so that the computer can keep track of what it needs to do when it gets back.

By contrast, calculating `f(x)` looks something like:

    f(x): x -> x-1 -> f(x-1)

The computer can happily move on, calculate `f(x-1)`, and not worry at all about getting back and doing something with the result. It doesn't have to keep "Oh, before this I was in the middle of something" on the stack because it wasn't in the middle of something, it was all done. The Haskell compiler takes advantage of this to optimize tail recursive functions.

Using an accumulating parameter is a nice trick to make g into a tail recursive function. What we do is create a new function `g'` which has two parameters instead of one. The second is the accumulating parameter which keeps track of the `+x` part. So `g'(x,acc) = g'(x-1,acc+x)` and `g'(0,acc) = acc`. Since the accumulating parameter is not a natural part of the function `g`, we use a wrapper: `g(x) = g'(x,0)`.
