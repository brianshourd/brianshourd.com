---
title: Thing I Learned Today: Foldable
author: Brian Shourd
date: January 15, 2013
tags: coding, haskell
math: true
---

It's high time I explored this
[`Foldable`](http://www.haskell.org/ghc/docs/latest/html/libraries/base/Data-Foldable.html)
typeclass in Haskell. It's a typeclass for data structures that can be
folded up, compressed down to a single value in a meaningful way. Start
with a data type constructor `t`, of kind `* -> *`. The same kind of
constructor required to be a `Functor` or `Monad`. Then we have the
typeclass `Foldable`, with methods

~~~{.haskell}
class Foldable t where
    fold :: Monoid m => t m -> m
    foldMap :: Monoid m => (a -> m) -> t a -> m
    foldr :: (a -> b -> b) -> b -> t a -> b
    foldr' :: (a -> b -> b) -> b -> t a -> b
    foldl :: (a -> b -> a) -> a -> t b -
    foldl' :: (a -> b -> a) -> a -> t b -> a
    foldr1 :: (a -> a -> a) -> t a -> a
    foldl1 :: (a -> a -> a) -> t a -> a
~~~

That's a lot of methods! What do we need to define in order to create a
minimum definition?

Only `foldMap` or `foldr`.

Really? That seems pretty amazing. In fact, it seems pretty astounding
that we can get either of `foldMap` or `foldr` from the other. Let's see
how it's done, then.

~~~{.haskell}
foldMap :: Monoid m => (a -> m) -> t a -> m
foldMap f = foldr (mappend . f) mempty
~~~

Ok, so if `foldr` is defined, then we can use the monoid structure of
`m` to create `foldMap`. Let's look at an example: if `t` is the type
`[]` and `m` is also `[]`, then `foldMap` becomes

~~~{.haskell}
foldMap f = foldr ((++) . f) []
~~~

So

~~~{.haskell}
foldMap f [1,2,3,4] = foldr ((++) . f) [] [1,2,3,4]
                    = (f 1) ++ (f 2) ++ (f 3) ++ (f 4)
                    = concat [f 1, f 2, f 3, f 4]
                    = concat $ map f $ [1,2,3,4]
                    = concatMap f [1,2,3,4]
~~~

Ok, then, I see. But how is `foldr` defined in terms of `foldMap`, then?

~~~{.haskell}
foldr :: (a -> b -> b) -> b -> t a -> b
foldr f z t = appEndo (foldMap (Endo . f) t) z
~~~

What is this `Endo`?

~~~{.haskell}
newtype Endo a = Endo { appEndo :: a -> a }
instance Monoid (Endo a) where
    mempty = Endo id
    (Endo f) `mappend` (Endo g) = Endo (f . g)
~~~

It's just a type encoding the fact that endomorphisms create a monoid
under composition of functions. That means that `Endo . f` is of type `a
-> Endo b`. This actually makes great sense - when we `foldr`, we use
the function `f :: a -> (b -> b)` to transform all of the objects in `t
a` to endomorphisms, then compose them according to `foldMap` and apply
them to the argument given.

Let's get a bit more specific. The source for `Data.Foldable` includes
instances for both `Foldable Maybe` and `Foldable []`.

~~~{.haskell}
instance Foldable Maybe where
    foldr _ z Nothing = z
    foldr f z (Just x) = f x z

    foldl _ z Nothing = z
    foldl f z (Just x) = f z x
~~~

This is pretty straightforward. We define `foldr` so that `Nothing`
always becomes the identity map, and then is applied to `z`. If we don't
have `Nothing`, then we have `Just x`, so we get `f x z`. We also define
`foldl`, presumably for speed, since it does exactly the same thing,
only with a different argument order.

What, then, does `foldMap` do when our type is `Maybe`? Let's try some
examples:

~~~{.haskell}
foldMap :: Monoid m => (a -> m) -> t a -> m
foldMap f = foldr (mappend . f) mempty

foldMap f Nothing  = foldr (mappend . f) mempty Nothing
                   = mempty
foldMap f (Just x) = foldr (mappend . f) mempty (Just x)
                   = mappend . f x $ mempty
                   = (f x) `mappend` mempty
                   = f x
~~~

With lists, we are familiar with how `foldr` works, but let's look at
the implementation.

~~~{.haskell}
Instance Foldable [] where
    foldr k z = go
          where
            go []     = z
            go (y:ys) = y `k` go ys
~~~

This is (apparently) a common idiom in Haskell, using a function called
`go` to encapsulate the recursion. 

Then `foldMap` on lists becomes

~~~{.haskell}
foldMap f (x:xs) = foldr (mappend . f) mempty (x:xs)
                 = (mappend . f) x (foldr (mappend . f) mempty xs
                 = (f x) `mappend` foldMap f xs
~~~

We apply `f` to every element of the list, and `mappend` the results
together. We could use this to, e.g. get the nth approximation of the
power series of $e^x = \sum_{i=0}^{\infty} \frac{x^n}{n!}$.

~~~{.haskell}
import Data.Monoid 
import Data.Foldable

pSeries :: Double -> Int -> Double
pSeries x k = getSum $ foldMap f [0..k] where
    f n = Sum (x^n / (fromIntegral $ Prelude.product [2..n]))

ghci> pSeries 0.5 10
1.6487212706873655
ghci> exp 0.5
1.6487212707001282
~~~

We could probably use it for something a lot less superficial, too.

That's a quick look at the `Foldable` type for today. I'll have to take
a look at the rest of the functions in `Data.Foldable` another day.
