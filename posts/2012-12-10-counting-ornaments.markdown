---
title: Counting Ornaments
author: Brian Shourd
date: December 10, 2012
tags: math
math: true
---

Or

##This is Your Brain on Math

My daughter loves the "IF You Give..." series by Laura Numeroff,
especially [Merry Christmas,
Mouse!](http://www.amazon.com/Merry-Christmas-Mouse-You-Give/dp/0061344990)
('tis the season). While reading it to her for the hundredth time or so,
I came across a simple, fun math puzzle hidden within its pages.

On each page of the book, Mouse is putting a type of ornaments on the
tree, in increasing number. First, he adds 1 star. Then 2 snowflakes,
followed by 3 reindeer, and so on until he gets to 10 rockets. At this
point, the book shows a massive pile of ornaments, stating that mouse
has "only 100 more to go", the impact being "wow, that mouse sure loves
ornaments, because 100 is a lot!"

But how many did he already put up?

$$1 + 2 + 3 + ... + 10 = ?$$

This is an [arithmetic
sequence](http://en.wikipedia.org/wiki/Arithmetic_progression), and due
to years of training for math competitions, I can't see one of these
without compulsively finding the partial sums. The formula is simple -
to add up the numbers 1 through 10, we pair them up, and multiply by the
number of pairs. $1 + 10 = 11$, and $2 + 9 = 11$, and $3 + 8 = 11$, and
$4 + 7 = 11$, and $5 + 6 = 11$, so that's five pairs, each adding up to 11. 
That totals 55: $1 + 2 + ... + 10 = 55$.

By the time that there are 100 ornaments left, mouse has already put 55
on! He's actually over 33% done already, even though 100 sounds like so
many.

This sparks another question: if he continues in this fashion (next
addint 11 of something, then 12 of something else, etc.), how many
different kinds of ornaments will be on the tree? Are there exactly 100
left, or is that just an approximation?

In other words, find $n$ such that $1 + 2 + 3 + ... + n = 155$, if such
an $n$ exists, or find the best $n$ so that the sum is close. Well,
following the logic above, each pair has sum $n + 1$, and there are
$n/2$ pairs, so we want to find $n$ such that

$$\frac{n (n+1)}{2} = 155$$

Simplifying yields

$$n^2 + n - 310 = 0$$

A [quadratic](http://en.wikipedia.org/wiki/Quadratic_equation)! We can
solve it either with the quadratic formula, or by [completing the
square](http://www.purplemath.com/modules/sqrquad.htm), which is my
personal favorite. In either case, we find that there are actually two
solutions (we should expect this with quadratics): $n = \frac{1}{2} (-1 \pm \sqrt{1241})$
That is, $n$ is a little more than $17$ or a little less than $-18$ The
negative answer doesn't make sense in the context of our problem, so our
solution is close to 17. Again, we were looking for a whole number, and
the closest one is 17, in which case there are exactly $1 + 2 + ... + 17 = \frac{17 \times 18}{2} = 153$ 
ornaments on the tree.

Either Mouse breaks his pattern, or he was exaggerating the work he had
left!

Ok, so that was fun, right? But I had to use a calculator (or at least
some scrap paper) to figure that out, right? Well, yes and no.

If you want to exactly recreate the stuff I just did, then probably yes
(unless you are just really good at doing in-your-head arithmetic). But
not if you work a bit smarter.

When we got the the equation $\frac{n (n+1)}{2} = 155$, we can either
solve this explicitely, or guess at an integer solution by
approximating. We're looking for a positive integer such that $n^2 + n =
310$, right? Well, for large $n$, $n^2 + n$ is basically just $n^2$. So
we just want to find an integer approximation for the square root of
$310$. We know $15^2 = 225$ and $20^2 = 400$, so it's somewhere in the
middle - my next guess was $17$, and $\frac{17 \times 18}{2} = 17 \times 9 = 10 \times 9 + 7 \times 9 = 90 + 63 = 153$, which is close enough (and all in our heads!).

These are the sort of things that happen when you spend a huge chunk of
your junior-high/high-school/undergraduate years training for math
competitions - you start to see interesting problems everywhere. If you
like these kinds of problems, I highly suggest taking a course in
Discrete Math. I'm just noticing that neither Coursera nor Udacity seems
to have one right now. Somebody needs to fill that gap, Discrete was my
very favorite undergrad course!
