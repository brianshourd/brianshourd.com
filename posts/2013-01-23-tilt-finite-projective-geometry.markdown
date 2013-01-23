---
title: Thing I Learned Today: Finite Projective Geometry
author: Brian Shourd
date: January 23, 2013
tags: math, coding
math: true
---

Today I learned about a math construct called *finite projective
geometry* in the coding theory course that I'm taking. Maybe you are
already familiar with [Euclid's
Axioms](http://en.wikipedia.org/wiki/Euclidean_geometry#Axioms), where
Euclid set down an axiomatic list of properties, and then set to prove a
ridiculous amount of things based on those five simple properties.

Finite projective geometry is similer, except that there are only three
axioms. We start with three sets:

* $L$ - the so-called set of lines
* $P$ - the set of points
* $I \subset P \times L$ - the incidence set

[If you aren't familiar with the terminology $P \times L$, it simply
means the collection of pairs $(p,l)$ where $p \in P$ and $l \in L$. If
both $P$ and $L$ are finite, then the number of elements in $P \times L$
is the number of elements in $P$ times the number of elements in $L$.]

If a pair $(p,l)$ is in $I$, we say that $p$ belongs to $l$. Be careful
- we haven't defined what points or lines are, outside of the fact that
they are elements of named sets. In particular, you cannot be thinking
of lines and points in space, as you usually do. Abandon all intuition,
ye who enter here.

These sets $P$, $L$, and $I$ form a *finite projective geometry* if they
satisfy the following axioms:

1. For every two distinct points, there is exactly one line containing
   both of them.
2. For every two distinct lines, there is exactly one point contained in
   both of them.
3. There exist some four points such that no three belong to the same
   line.

That's it. What are some consequences? There are no parallel lines,
according to axiom 2, since every pair of lines must intersect. There
can't be fewer than four points, by axiom 3.

Actually, what is the fewest number of points that we can have? Let's
try four. We have four points, $a$, $b$, $c$, and $d$. First, by axiom
1, there must be lines $ab$, $ad$, and $ac$. Moreover, none of these are
the same line, else they would contain three points, violating axiom 3.

![Four points, three lines](/images/fpg1.jpg)

Similarly, we must have lines $bd$, $cd$, and $bc$, each different.

![Four points, six lines](/images/fpg2.jpg)

Are we done? Not quite, we aren't currently satisfying axiom 2, since
the lines $bd$ and $ac$ don't intersect (they look like they do, but
notice that there isn't a single point contained in both of them). We're
stuck - we can't extend any of our lines without violating axiom 3, and
we can't add any more lines without violating axiom 1. The only thing we
could do is add more points.

So there aren't any examples of a finite projective geometry with four
points.

We can add a point $e$ on the intersection of $ac$ and $bd$. But $ab$ and
$cd$ don't cross, and neither do $bc$ and $ad$. So we'll add two more
points, $g$ and $f$, and extend these lines.

![Seven points, six lines](/images/fpg3.jpg)

Are we done yet? No! There is no line connecting $f$ and $g$, or $f$ and
$e$, or $e$ and $g$. So we add a line containing all of them.

![Seven points, seven lines](/images/fpg4.jpg)

And it looks like we're done! Every pair of points is connected by a
line, every pair of lines intersect at exactly one point, and there are
four points ($a$, $b$, $c$, and $d$) of which no three are colinear. We
can re-arrange this for a slightly more appealing shape.

![Fano plane](/images/fpg5.jpg)

This is called the [Fano
plane](http://en.wikipedia.org/wiki/Fano_plane), and aside from being
pretty cool to look at, it actually has a use in coding theory.

We can rename these points again with numbers in binary from 1 to 7.

![Fano plane, binary](/images/fpg6.jpg)

Notice that if we perform bitwise addition on any two numbers on a line,
we get the third. This is actually the three dimensional vector space
over the field of characteristic two (excluding the origin). The basis
vectors are $001$, $100$, and $010$ - the corners of the triangle. Of
course, we could draw this triangle other ways, and we could also use a
different basis.

How does this give us a code? Well, we form what is called the incidence
matrix, the matrix $A$ whose $i,j$ entry is just one if point $i$
belongs to line $j$, and zero otherwise. The columns of this matrix are
essentially binary strings of length 7, three of which are 1 and four of
which are zero. We'll call these columns $v_1, \ldots, v_7$. Each of
these has a complement, where we swap all the $1$s for $0$s and vice
versa, which we will call $\bar{v_1}, \ldots \bar{v_7}$. If we also
include the string of all zeros and the string of all ones, we get 16
total strings.

Here's what's neat: this particular code is a [perfect
code](http://en.wikipedia.org/wiki/Hamming_bound#Perfect_codes).
Loosely, that means that it fits the vectors together in such a way that
it is easy to correct errors in transmitting the code. Indeed, if we
consider the distance between two vectors to be the number of bit flips
necessary to get from one to another, then every two vectors of the 16 I
just described are at least 3 apart.

What do I mean? Imagine that you have 16 different signals that you want
to be able to send. You can encode these digitally as strings of 1s and
0s, but you know that the channel that you send your data over might
just corrupt your data (flip some bits). If you encode it with four
binary digits, then a single bit flip will result in your signal being
decoded as the wrong signal at the other end. But, if you map each
signal to one of the 16 vectors given above, and then transmit these
(now 10 bits of data), it will take no fewer than three bit flips before
a given signal is transformed into a different signal.

Better still - if your channel is reliable enough that you don't expect
a lot of bit flips, you can set it up so that your reciever can
*correct* a single bit flip without needing to retransmit the data.

The fact that this code is *perfect* means that it occupies some kind of
local maximum for packing this error-detecting-correcting capability
into a small number of bits, relative to the number of words that you
want to send (16) and the number of characters in your alphabet (2).
It's very interesting stuff.
