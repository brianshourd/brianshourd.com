---
title: Thing I Learned Today: The Wonderful Compactification
author: Brian Shourd
date: October 10, 2012
tags: lie theory, math
math: true
---

A recent comment on this blog showed me that I haven't updated this blog in a long time. Part of that is because I have no vision for it.

So here's my vision: every work day (at least) I'll post something that I learned that day. Because I learn something new every day. That's most of what I do. Some of these things might be interesting to some people sometimes, but the real reason is so that I get practice talking about the stuff I'm learning, and to encourage myself to **actually learn something new every day.**

To reiterate: I hope you find these interesting, but since I am deep in my research for my thesis right now, I think that many of them will revolve around somewhat advanced mathematics (read: abstract nonsense). That doesn't mean that it is hard, just that it requires a lot of unfortunate jargon. I'll try to post links for background - maybe check out some constructions that you've never seen before!

* * *

Today I learned about the construction of the [Wonderful Compactification](http://arxiv.org/abs/0801.0456) (disclaimer: linked article is co-authored by my advisor). It's apparently an extremely useful method for taking an [algebraic group](http://en.wikipedia.org/wiki/Algebraic_group) (technically, a complex connected semisimple algebraic group of adjoint type) and embedding it into a [smooth projective variety](http://en.wikipedia.org/wiki/Projective_variety#Smooth_projective_varieties) (which are nice for lots of reasons - many of which I don't know about).

Here's how it works: let $G$ be the group in question. Pick an irreducible representation $V$ of $G$ having highest weight $\lambda$ ($\lambda$ should be regular). Any old representation will do. Define a $G \times G$ action on $\mathbf{P} (\textit{End} V)$ (that's the projectivisation of the space of endomorphisms of $V$) by $(g_1, g_2) \cdot \left[ A \right] = \left[ g_1 A g_2^{-1} \right]$. While you're at it, consider the $G \times G$ action on $G$ given by $(g_1, g_2) \cdot x = g_1 x g_2^{-1}$.

Then consider the map $\psi \, : \, G \rightarrow \mathbf{P} (\textit{End} V)$ given by $\psi (g) = \left[ g \right]$ (technically, this is $\left[ \pi (g) \right]$ where $\pi \, : \, G \rightarrow \textit{End} V$ is the representation of $G$). Notice that this map is $G \times G$ equivariant.

Then the wonderful compactification $X$ of $G$ is given by $X = \overline{\psi (G)}$. Equivalently, since $G = (G \times G) \cdot 1$ and $\psi$ is $G \times G$ equivariant, it is given by $X = \overline{(G \times G) \cdot \left[ \textit{id}_V \right]}$. The latter approach is particularly nice since it shows us that $X$ is actually the closure of an orbit, and therefore $G$ is open in $X$.

One of the really cool things about this construction is that it doesn't depend on the choice of representation (in fact, later in the paper it is constructed without even choosing a representation). This is pretty much the first construction that I've ever seen which uses a representation but is independent of the choice of representation.

* * *

And that wraps up my thing I learned today. Today's was a bit hard to understand, unfortunately, but at least it was short! I promise tomorrow's will be significantly less esoteric.
