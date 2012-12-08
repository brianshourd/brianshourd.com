---
title: Thing I Learned Today: The 3SAT Problem
author: Brian Shourd
date: October 30, 2012
tags: math
math: true
---

Today's post is short: I learned about the [3SAT
Problem](http://en.wikipedia.org/wiki/Boolean_satisfiability_problem#3-satisfiability).
The term "3SAT" stands for "Boolean Satisfyability Problem where each
clause contains 3 variables". Here is the problem:

Given an expression of the form $(y_{i_1} \vee y_{i_2} \vee y_{i_3}) \wedge (y_{i_4} \vee y_{i_5} \vee y_{i_6}) \wedge \ldots \wedge (y_{i_{n-2}} \vee y_{i_{n-1}} \vee y_{i_n})$ where each $y_{i_k}$ is one of the variables $x_1, x_2, \ldots, x_m$ or $\neg x_1, \neg x_2, \ldots, \neg x_m$ (where $\neg$ is logical negation), is there a way to assign `TRUE` or `FALSE` to each $x_i$ so that the entire expression evaluates to `TRUE`?

This family of problems is
[NP-Complete](http://en.wikipedia.org/wiki/NP-complete), which
informally means that they are really hard to solve in general. That is,
all known algorithms for solving all 3SAT problems increase in the time
they take to work quickly when more and more variables are added. A
specific 3SAT problem might be really easy to solve, or even entire
classes of 3SAT problems. But it is definitely not easy to solve all
3SAT problems with a single, generic, algorithm.
