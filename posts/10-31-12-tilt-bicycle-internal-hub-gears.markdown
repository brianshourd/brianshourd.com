---
title: Thing I Learned Today: Bicycle Internal Hub Gears
author: Brian Shourd
date: October 31, 2012
tags: engineering
math: true
---

Today I learned how [internal hub gears](http://sheldonbrown.com/internal-gears.html) on bicycles work. A hub gear is a method of creating multiple gears on a bicycle without having an external derailleur and multiple sprockets. Why? Well, keeping a derailleur system lubricated, clean, and tuned-up is a lot of work. But if we could move all of that nonsense into a sealed container **inside** the hub of the wheel and permanently seal it, the bike rider wouldn't have to worry about it. In addition, it's much less vulnerable to damage while riding.

So why aren't they more widely used? For one thing, they don't offer as much of a range as a standard derailleur system - the best one only has 14 gears at the time of this writing, and many only have three. This makes them less efficient for experienced riders (you know, the people who spend the most money on bikes). Also, they are more expensive.

But the way in which they work is remarkably simple. In the three-speed case, it's just a [planetary gear system](http://en.wikipedia.org/wiki/Epicyclic_gearing). In cases with more gears, planetary gears are used in sequence. We'll just talk about the basic three-speed version.

A simple planetary gear system looks like the picture below:

![Planetary Gears](images/planetary.jpg)

There is a sun gear, some inner gears (also called planets or middle gears), and an outer ring gear (which has teeth on the inside). The sun and planets are connected by a planet carrier in such a way that they spin freely with respect to the carrier. Such a planetary gear system can be used in many different styles, depending on whether the sun, ring, or carrier is fixed and which of these is used as input.

On a three-speed bicycle hub gear system, the sun gear is fixed to the axle of the bike (which in turn is rotationally fixed to the bike frame via special washers), and the sprocket drives either:

* the wheel directly (as on a single speed bike),
* the planet carrier (in which case the ring gear drives the wheel), or
* the ring gear (in which case the carrier drives the wheel).

Let's figure out the math together. To set this up, suppose that the radii of the gears are $s$ (for the sun gear), $p$ (for the planetary gear), and $r$ (for the ring gear). In configuration 1, every turn of the sprocket results in exactly one turn of the wheel.

In configuration 2, notice that every time the planet carrier makes one rotation, the planets will roll a distance of $2 \pi s$, since they roll around the sun gear. Since they have circumference $2 \pi p$, they roll in total $\frac{2 \pi s}{2 \pi p} = \frac{s}{p}$ times. Similarly, for every rotation that a planet makes, the ring turns $\frac{p}{r}$ times. Thus each rotation of the the planet carrier results in $1 + \frac{p}{r} \cdot \frac{s}{p} = 1 + \frac{s}{r}$ rotations (the extra 1 comes from the fact that rotating the planet carrier once also rotates the ring). That means that a single rotation of the sprocket results in more than a single rotation of the wheel - making it *harder* to push the bike. In bike terms, this is a higher gear than configuration 1.

Configuration 3 is just the reverse of configuration 2, so we see that a single rotation of the sprocket results in $\frac{1}{1 + \frac{s}{r}} = \frac{r}{s + r}$ rotations of the carrier (and hence the wheel). This is less than $1$, so that means that in bike terms, this is a lower gear than configuration 1.

What luck! We naturally get three gears - an increase, a decrease, and a direct. By adjusting the size of the front and rear sprockets, we can choose our middle gear to be as high or as low as we want - regardless of the nature of the hub. Then, all that matters is the ratio $\frac{s}{r}$. This ratio will determine how much harder our high gear is, and how much easier our low gear is.
