---
title: Thing I Learned Today: Particle Swarm Optimization
author: Brian Shourd
date: January 7, 2013
tags: coding, haskell
---

Today I learned about [Particle Swarm
Optimization](http://en.wikipedia.org/wiki/Particle_swarm_optimization),
a technique for finding minimums/maximums of certain types of functions.
If you have heard of Genetic Algorithms, Particle Swarm Optimization is
another method to solve similar classes of problems (although the two
work very differently). Both are called _metaheuristics_, and they seem
to work well, though their effectiveness has not been mathematically
proven.

To understand it, I highly suggest the original paper by James Kennedy
and Russell Eberhart: [Particle Swarm
Optimization](http://www.cs.tufts.edu/comp/150GA/homeworks/hw3/_reading6%201995%20particle%20swarming.pdf).
The paper is only 7 pages and is quite readable. Go do it now.

However, if you didn't read it, here's a quick summary. Suppose that we
want to minimize the function `f (x,y) = x^2 + y^2`. This is a function
from a pair of real numbers to a single real number. So we consider a
bunch of particles flying around the plane. Each one has a location and
a velocity, which are updated in stages, and we think about them flying
around to find the best location for the swarm (where "best" means
"minimizes the function `f`").

How do they do that? Each particle knows the best location that it has
ever visited, and the best location that the entire swarm has
discovered, and flys towards them, with a bit of randomization to allow
them to overfly/underfly, thus discovering new candidate best points.

More precisely, here is some code for how a particle is updated:

~~~{.haskell}
import System.Random
-- Used as location and velocity values
type Point = (Double, Double)

data Particle = Particle
    Point       -- position of particle
    Point       -- velocity of particle
    Candidate   -- best position this particle has found
    deriving (Eq, Show)

-- Add two points as vectors
pAdd :: Point -> Point -> Point
pAdd (x1,y1) (x2,y2) = (x1+x2, y1+y2)

-- Subtract two points as vectors
pSubtract :: Point -> Point -> Point
pSubtract (x1,y1) (x2,y2) = (x1-x2, y1-y2)

-- Scale a point as a vector
pScale :: Double -> Point -> Point
pScale a (x,y) = (a * x, a * y)

-- A candidate solution with location and score at that location
type Candidate = (Point, Double)

-- Update a single particle by a single step
updateParticle :: Particle -> Candidate -> (Point -> Double) -> StdGen -> (Particle, StdGen)
updateParticle (Particle pos vel best) gBest f gen = (Particle pos' vel' best', gen'') where
    -- New position is the old position plus the new velocity
    pos' = pAdd pos vel'

    -- We need two random numbers
    (r1, gen') = randomR (0,1) gen :: (Double, StdGen)
    (r2, gen'') = randomR (0,1) gen' :: (Double, StdGen)

    -- The vector representing the motion from current location
    -- to the best locations
    dp = pSubtract (fst best) pos
    dg = pSubtract (fst gBest) pos

    -- New velocity is a weighted average of local and global bests,
    -- affected by current velocity
    vel' = vel `pAdd` (r1 `pScale` dp) `pAdd` (r2 `pScale` dg)

    -- Update local best if we've found a better one
    best' = case (compare (snd best) (f pos')) of
        LT -> best
        _  -> (pos', f pos')
~~~

All that remains is to define and update an entire swarm of these
things. Before we do, though, notice that we haven't really used any
properties of `(Double, Double)` other than the fact that we can add,
subtract, and multiply `Point`s by scalars (these are, roughly speaking,
the exact requirements that make the collection of all `Point`s a
[Vector Space](http://en.wikipedia.org/wiki/Vector_space)). In order to
create `Particle`s with zero velocities (as when we initialize a swarm),
we also need a `pZero` function (another property of vector spaces).

So we should be able to abstract this away into a typeclass, which
changes our code slightly. Here it is, updated.

~~~{.haskell}
-- Represents the position and velocity of a particle
-- The four functions are necessary in order
-- to use Points to represent positions and velocities
-- of particles (so we can update a position based on
-- its velocity, update its velocity semi-randomly, etc.)
class (Eq a, Show a) => PSOVect a where
    pAdd :: a -> a -> a
    pScale :: Double -> a -> a
    pSubtract :: a -> a -> a
    pZero :: a
    pSubtract v1 v2 = pAdd v1 $ pScale (-1) v2

-- A candidate for a global minimum. Stores both the
-- location of the possible minimum, and the value of
-- the function to minimize at that point.
data PSOCand a = PSOCand {
    pt :: a, 
    val :: Double
    } deriving (Show, Eq)

-- Better/worse candidates are determined by their values
instance (PSOVect a) => Ord (PSOCand a) where
    compare = compare `on` val

-- Particles know their location, velocity, and the
-- best location/value they've visited. Individual
-- particles do not know the global minimum, but the
-- Swarm they belong to does
data Particle a = Particle {
    pos :: a,       -- position of particle
    vel :: a,       -- velocity of particle
    pBest :: PSOCand a  -- best position this particle has found
    } deriving (Eq, Show)

-- Compare points based on their best value
instance (PSOVect a) => Ord (Particle a) where
    compare = compare `on` (val . pBest)
~~~

Then we can make `Point` an instance of this typeclass.

~~~{.haskell}
type Point = (Double, Double)

-- Make it a PSOVect, to use in the algorithm
instance PSOVect Point where
    pAdd (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)
    pScale r (x,y) = (r * x, r * y)
    pZero = (0,0)

-- Let us randomly generate points, in order to randomly
-- create a Swarm
instance Random Point where
    random g = ((x, y), g'') where
        (x, g')  = random g
        (y, g'') = random g'
    randomR ((a1, b1), (a2, b2)) g = ((x, y), g'') where
        (x, g')  = randomR (a1, a2) g
        (y, g'') = randomR (b1, b2) g'
~~~

However, it should be clear that we could also make, e.g., `Float`,
`[Double]`, or `(Double, Double, Float)` an instance of the `PSOVect`
typeclass if we wanted. We could even have some discrete components like
`(Int,Double)` an instance using rounding in the `pScale` function. I'm
fairly sure that Particle Swarm Optimization will still be an effective
method under such conditions.

With these definitions, what should a swarm look like? It needs to hold 
a collection of particles, the function that these particles wish to 
minimize, and the global best `PSOCand`.

~~~{.haskell}
-- A Swarm keeps track of all the particles in the swarm,
-- the function that the swarm seeks to minimize, and the
-- best location/value found so far
data Swarm a = Swarm {
    parts :: [Particle a],  -- particles in the swarm
    gBest :: PSOCand a,     -- best position found
    func :: a -> Double,    -- funtion to minimize
    }

instance (PSOVect a) => Show (Swarm a) where
    show (Swarm ps b _ _ _) = (show $ map pBest $ ps) ++ (show b)
~~~

And then some simple functions to initialize a `Swarm` and update a
`Swarm`.

~~~{.haskell}
-- Create a swarm in initial state based on the
-- positions of the particles and the grading
-- function. Initial velocities are all zero.
createSwarm :: (PSOVect a) => [a] -> (a -> Double) -> Swarm a
createSwarm ps f = Swarm qs b f where
    qs = map (createParticle f) ps
    b = pBest . minimum $ qs
    createParticle f' p = Particle p pZero (PSOCand p (f' p))

-- Update the swarm one step, updating every 
-- particle's position and velocity, and the
-- best values found so far
updateSwarm :: (PSOVect a) => Swarm a -> StdGen -> (Swarm a, StdGen)
updateSwarm s@(Swarm ps b f) g = (Swarm qs b' f, g') where
    (qs, g', b') = foldl' helper ([], g, b) ps
    helper (acc, gen, best) p = (p':acc, gen', min best (pBest p')) where
        (p',gen') = updateParticle p s gen
~~~

The `updateSwarm` function deserves some comment. Initially, it seems
that the proper way to update the swarm is to first map `updateParticle`
over all the particles, then look for the new global best. There are two
problems with this, however. The first is that we thread a `StdGen`
through `updateParticle`, meaning we can't actually use `map`. There are
ways around this, of course. The second reason is that it is ineffecient
to traverse the entire list of particles twice. Both of these can be
solved by using the slightly less natural `fold` instead of `map`.

At least, my naive knowledge of Haskell led me to make this choice.

That's it! Our program should now run, and we have a somewhat general
framework for performing PSOs.

There are, however, two ways to make this even better. Normally, the
velocity of a particle is updated with some parameters - a inertia
parameter, and two parameters relating the tendancy of particles to seek
global or local values more fervently. In the original paper, these are
all constants, and indeed are all 1. However, in later papers, and in
particular in [Parameter Selection in Particle Swarm
Optimization](http://www.cs.tufts.edu/comp/150GA/homeworks/hw3/_reading6%201995%20particle%20swarming.pdf)
by Yuhui Shi and Russell C. Eberhart, these become functions which
change with "time". That is, these parameters are actually functions
which depend on the number of steps we've taken.

Some new typeclasses, some changes to `Swarm` and `updateParticle`, and
we're on our way.

~~~{.haskell}
-- Holds the parameters used to update particle 
-- velocities, see "Parameter Selection in Particle
-- Swarm Optimization" by Yuhui Shi and 
-- Russell C. Eberhart
data PSOParams = PSOParamsStatic
    Double  -- inertia weight
    Double  -- tendancy toward local
    Double  -- tendancy toward global
    | PSOParamsDynamic 
    (Integer -> Double)
    (Integer -> Double)
    (Integer -> Double)

-- The original parameters given in the 1995 paper
-- "Particle Swarm Optimization" by James Kennedy 
-- and Russell Eberhart
defaultPSOParams :: PSOParams
defaultPSOParams = PSOParamsStatic 1 2 2

-- A Swarm keeps track of all the particles in the swarm,
-- the function that the swarm seeks to minimize, and the
-- best location/value found so far
data Swarm a = Swarm {
    parts :: [Particle a],  -- particles in the swarm
    gBest :: PSOCand a,     -- best position found
    func :: a -> Double,    -- funtion to minimize
    params :: PSOParams,    -- parameters
    iteration :: Integer    -- current iteration
    }

instance (PSOVect a) => Show (Swarm a) where
    show (Swarm ps b _ _ _) = (show $ map pBest $ ps) ++ (show b)

-- Create a swarm in initial state based on the
-- positions of the particles and the grading
-- function. Initial velocities are all zero.
createSwarm :: (PSOVect a) => [a] -> (a -> Double) -> PSOParams-> Swarm a
createSwarm ps f pars = Swarm qs b f pars 0 where
    qs = map (createParticle f) ps
    b = pBest . minimum $ qs
    createParticle f' p = Particle p pZero (PSOCand p (f' p))

-- Update the swarm one step, updating every 
-- particle's position and velocity, and the
-- best values found so far
updateSwarm :: (PSOVect a) => Swarm a -> StdGen -> (Swarm a, StdGen)
updateSwarm s@(Swarm ps b f pars i) g = (Swarm qs b' f pars (i+1), g') where
    (qs, g', b') = foldl' helper ([], g, b) ps
    helper (acc, gen, best) p = (p':acc, gen', min best (pBest p')) where
        (p',gen') = updateParticle p s gen

-- Update a particle one step. Called by updateSwarm
-- and requires the swarm that the particle belongs
-- to as a parameter
updateParticle :: (PSOVect a) => Particle a -> Swarm a -> StdGen -> (Particle a, StdGen)
updateParticle (Particle p v bp) (Swarm ps b f pars i) g = (Particle p' v' bp', g'') where
    p' = pAdd p v'
    (r1, g') = randomR (0,1) g :: (Double, StdGen)
    (r2, g'') = randomR (0,1) g' :: (Double, StdGen)
    dp = pSubtract (pt bp) p
    dg = pSubtract (pt b) p
    v' = newVel pars
    newVel (PSOParamsStatic omega c1 c2) = pAdd (pScale omega v) $ 
         pAdd (pScale (c1 * r1) dp) $
         (pScale (c2 * r2) dg)
    newVel (PSOParamsDynamic omega c1 c2) = pAdd (pScale (omega i) v) $ 
         pAdd (pScale ((c1 i) * r1) dp) $
         (pScale ((c2 i) * r2) dg)
    bp' = min bp $ PSOCand p' (f p')
~~~

Now we are really ready for anything, so let's make a simple module and
put our code to the test. I've gone ahead and put this code up on
Github:
[haskell-ParticleSwarmOptimization](https://github.com/brianshourd/haskell-ParticleSwarmOptimization). It may have changed since I wrote the above, but those are the basics of PSO.

Actually, there's lots more to say, but at this point I've written about
the "thing I learned today" for upwards of two weeks. Time to print!

