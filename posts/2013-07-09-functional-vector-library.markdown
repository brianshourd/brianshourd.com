---
title: Designing a Vector Module in JS Using Functional Techniques
author: Brian Shourd
date: June 10, 2013
tags: coding, javascript
---

In this post, I'll show you how to use some techniques from functional
programming (using the [Underscore.js](http://underscorejs.org) library)
to create a simple vector module in JavaScript ([link to code on
GitHub](https://github.com/brianshourd/js-vector)). By vector module, I
mean a module that lets you do elementary linear algebra on tuples of
numbers - something that lets you add `[1, 3]` to `2 * [-1, 0]` to get
`[-1, 3]`, or multiply vectors by matrices.

*Disclaimer*: I'm ignoring all of the condition checks that probably
should be included in a production library. I'm leaving them out for
clarity, so that the code isn't bogged down by mundane checks - the
purpose here is to display some interesting code, not provide a safe,
feature complete module.

## A Vector Module

As a first step, let's define

~~~
var Vect = (function() {
    var vect = {};

    vect.create = function(x, y) {
        return [x, y];
    };

    return vect;
}());
~~~

We're just wrapping up an array.

~~~
var x = Vect.create(1, 3);  // [1, 3]
var y = Vect.create(-1, 0); // [-1, 0]
~~~

Then it is easy to define all the operators we need, like addition,
scalar multiplication, and subtraction.

~~~
vect.add = function(v, w) {
    return [v[0] + w[0], v[1] + w[1]];
};

vect.subtract = function(v, w) {
    return [v[0] - w[0], v[1] - w[1]];
};

vect.scale = function(a, v) {
    return [a * v[0], a * v[1]];
};
~~~

We've accomplished our goal! To do the operation I started with, just
use

~~~
console.log(Vect.add(x, Vect.scale(2, y))); 
    // => [-1, 3]
~~~

[Link to code so
far](https://github.com/brianshourd/js-vector/blob/627fcd91681a535511e64c46b83587af8c9f79f4/vect.js)

I can even perform more complicated expressions, like `[1, 1] + 2 * [3,
0] - 7 * [1, -1] + [0, 2]`:

~~~
var result = Vect.add(Vect.add([1, 1], Vect.scale(2, [3, 0])), Vect.subtract([0, 2], Vect.scale(7, [1, -1])));
console.log(result); // => [0, 10]
~~~

This reveals the first problem - this is horrible to look at, easy to
misplace parentheses, and just difficult to read. There is also a second
problem hidden in here. Each call to `Vect.add`, `Vect.subtract`, and
`Vect.scale` is creating a new array object. This call alone creates 5
different array objects, when we will discard all but one. I'm no expert
in optimization, but that seems like it could be a problem.

### Refactor

Suppose we needed to know this value a lot, but for different inputs.
That is, we want a function which will calculate `w + 2 * x - 7 * y + z`
for any vectors `w`, `x`, `y`, and `z`. We could write it as above

~~~
function f1(w, x, y, z) {
    return Vect.add(Vect.add(w, Vect.scale(2, y)), Vect.subtract(z, Vect.scale(7, y)));
}
~~~

but it is easier to read as

~~~
function f2(w, x, y, z) {
    return [
        w[0] + 2 * x[0] - 7 * y[0] + z[0],
        w[1] + 2 * x[1] - 7 * y[1] + z[1]];
}
~~~

Layed out like this, it is suddenly very easy to read our function. But
there is some obvious code duplication that should be removed
([DRY](http://en.wikipedia.org/wiki/Don%27t_repeat_yourself)).

~~~
function f3(w, x, y, z) {
    var temp = function(w, x, y, z) {
        return w + 2 * x - 7 * y + z;
    };
    return [
        temp(w[0], x[0], y[0], z[0]),
        temp(w[1], x[1], y[1], z[1])];
}
~~~

This removes some of the duplication, but not all. We're still manually
indexing and manually ordering our inputs, which is prone to error.

~~~
function f4(w, x, y, z) {
    var temp = function(w, x, y, z) {
        return w + 2 * x - 7 * y + z;
    };
    return _.map([0, 1], function(index) {
        return temp.apply(null, _.map([w, x, y, z], function(ary) {
            return ary[index];
        }));
    });
}
~~~

[Link to code so
far](https://github.com/brianshourd/js-vector/blob/8341943d155f2e329215c662aaff467e10d3382a/vect.js)

This is a bit hard to look at, and even harder to remember what to do! I
don't want to have to do a version of this every single time that I have
to make a function which is a linear combination of four vectors. For
example, if I instead want a function `g(w, x, y, z)` which returns `w + 4
* x + 5 * y + 6 * z`, I could write

~~~
function g1(w, x, y, z) {
    var temp = function(w, x, y, z) {
        return w + 4 * x + 5 * y + 6 * z;
    };
    return _.map([0, 1], function(index) {
        return temp.apply(null, _.map([w, x, y, z], function(ary) {
            return ary[index];
        }));
    });
}
~~~

Code duplication rears it's head again. This is identical to the code
for `f4`, except I've changed the `temp` function. We should write a
function to do this for us.

~~~
function makeWorkOnVectors(fun) {
    return function(w, x, y, z) {
        return _.map([0, 1], function(index) {
            return fun.apply(null, _.map([w, x, y, z], function(ary) {
                return ary[index];
            }));
        });
    };
}
~~~

Then we can easily write

~~~
var g2 = makeWorkOnVectors(function(w, x, y, z) {
    return w + 4 * x + 5 * y + 6 * z;
});

var f5 = makeWorkOnVectors(function(w, x, y, z) {
    return w + 2 * x - 7 * y + z;
});

console.log(g2([1,1], [3,0], [1,-1], [0,2])); // => [18, 8]
console.log(f5([1,1], [3,0], [1,-1], [0,2])); // => [0, 10]
~~~

[Link to code so
far](https://github.com/brianshourd/js-vector/blob/faa3c0196aa73d1406ad2d913a2d1940d8b2f7a2/vect.js)

Now that's visibility. 

### Introducing Vectorize

The only problem remaining is that `makeWorkOnVectors` only works on
functions that take 4 arguments. Which is a silly restriction anyway.
Let's remove that restriction and rename the function to
`Vect.vectorize`.

~~~
vect.vectorize = function(fun) {
    return function() {
        var args = _.toArray(arguments);
        return _.map([0, 1], function(index) {
            return fun.apply(null, _.map(args, function(ary) {
                return ary[index];
            }));
        });
    };
}
~~~

Some tests:

~~~
var f = Vect.vectorize(function(w, x, y, z) {
    return w + 2*x - 7*y + z;
};
var g = Vect.vectorize(function(w, x, y, z) {
    return w + 4*x + 5*y + 6*z;
};
var h = Vect.vectorize(function(x, y, z) {
    return x + y + z;
};

_.map([f, g, h], function(fun) {
    console.log(fun.call(null, [1,1], [3,0], [1,-1], [0,2]));
});
// => [0, 10]
// => [18, 8]
// => [5, 0]
~~~

[Link to code so
far](https://github.com/brianshourd/js-vector/blob/7821c89ea523314c41f7f8dad228fb0ad9c2206d/vect.js)

This is very readable, and also more efficient - I think that in a
single call to `g` (or `f` or `h`), only two arrays are created, and
we'll be keeping one of them. Whether or not this efficiency (in terms
of garbage created/collected) leads to actual performance gains, I do
not know. Think of it as green code.

## Secret Powers of Vectorize

We've just created a powerful tool. With a little extending, we'll
really take off.

### Variable-dimensional Vectors

First, we can fix a little thing that has been irking me. Our vectors
are all 2 dimensional! Which is silly, since they are arrays, and can be
any dimension at all. If we fix `vectorize` first, we can use it to
repair all the other functions. The change is extremely minor:

~~~
vect.vectorize = function(fun) {
    return function() {
        var args = _.toArray(arguments);
        return _.map(_.range(args[0].length), function(index) {
            return fun.apply(null, _.map(args, function(ary) {
                return ary[index];
            }));
        });
    };
}
~~~

We just changed the `[0,1]` (the array of indexes to use) to
`_.range(args[0].length)`, so that we use as many indexes as we have.
Then we can begin rewriting the rest of `Vect`.

~~~
vect.create = function() {
    return _.toArray(arguments);
};

vect.add = vect.vectorize(function(v, w) {
    return v + w;
});

vect.subtract = vect.vectorize(function(v, w) {
    return v - w;
});

vect.scale = function(a, v) {
    return vect.vectorize(function(v) { return a * v; })(v);
};
~~~

A little test:

~~~
var v = Vect.create(1, 2, 3);
var w = Vect.create(1, 1, 1);
console.log(Vect.add(v, w)); // => [2, 3, 4]
console.log(Vect.subtract(v, w)); // => [0, 1, 2]
console.log(Vect.scale(2, v)); // => [2, 4, 6]
~~~

[Link to code so
far](https://github.com/brianshourd/js-vector/blob/39958a14e8d45b995ffc1581a6506e2e115a1166/vect.js)

### Adding Better

What else can we do with this powerful version of `vectorize`? I think
that `Vect.add` is a bit limiting. It would be nice if it could take in
an arbitrary number of arguments. Let's create a function `sum` that
does just that, but for numbers.

~~~
var sum = function() {
    var args = _.toArray(arguments);
    return _.reduce(args, function(sum, current) {
        return sum + current;
    }, 0);
});

vect.add = vect.vectorize(sum);

var z = Vect.create(0, 0, 1);
console.log(Vect.add(v, w, z)); // => [2, 3, 5]
~~~

### Linear Combinations Revisited

After making so many linear combination functions (`f`, `g`, and `h`,
remember?), I'm finding it tedious to make more. Let's have a function do
that for us.

~~~
vect.lcom = function() {
    var coeffs = _.toArray(arguments);
    return vect.vectorize(function() {
        var inputs = _.toArray(arguments);
        return sum.apply(null, _.map(_.zip(coeffs, inputs), function(pair) {
            return pair[0] * pair[1]; // coeff * input
        }));
    });
};

var f = Vect.lcom(1, 2, -7, 1);
var g = Vect.lcom(1, 4, 5, 6);
var h = Vect.lcom(1, 1, 1);

_.map([f, g], function(fun) {
    console.log(fun.call(null, [1,1], [3,0], [1,-1], [0,2]));
});
console.log(h([1,1], [3,0], [1,-1]));
// => [0, 10]
// => [18, 8]
// => [5, 0]
~~~

[Link to code so
far](https://github.com/brianshourd/js-vector/blob/d35ccb90448868fb1c6c61381996109e6e199766/vect.js)

### Matrices

I'm bored. So what if linear combinations are easy? What value are they?
Well, if we remember our linear algebra, we know that matrix
multiplication is nothing but linear combinations. Math review: if I
have a matrix with columns `v` and `w`, and I multiply a vector `[1,2]`
by this matrix, the result is `1*v + 2*w`. It's a linear combination of
the columns of the matrix with coefficients given by the vector.

Thus if we agree to encode a matrix

~~~
1 2
3 4
~~~

as an array of column vectors

~~~
[[1,3], [2,4]]
~~~

we can write a function `vect.matrix`.

~~~
vect.matrix = function(m) {
    return function(v) {
        return vect.lcom.apply(null, v).apply(null, m);
    };
};

var m = Vect.matrix([[1, 3], [2, 4]]);
var x = [-1, 2];
console.log(m(x)); // => [3, 5]
~~~

Since matrices are literally just functions that take in a vector and
spit out a vector, we can even compose them!

~~~
var n = Vect.matrix([[1, 2, 3], [4, 5, 6]]);
var nm = _.compose(n, m);
console.log(nm(x)); // => [23, 31, 39]
console.log(n(m(x))); // => [23, 31, 39]
~~~

To get the coefficients of the product, multiply by the standard basis
vectors.

~~~
console.log(nm([1,0])); // => [13, 17, 21]
console.log(nm([0,1])); // => [18, 24, 30]
~~~

[Link to code so
far](https://github.com/brianshourd/js-vector/blob/d35ccb90448868fb1c6c61381996109e6e199766/vect.js)

That's it for today. I've shown you how a relatively simple functional
idea (vectorizing a function) can give us huge gains in code readability
throughout a vector module. The resulting library is small and readable
(only 48 lines of code - closure compiler reduces it to 268 bytes
gzipped), and includes features:

* Vectors of arbitrary dimension
* Simple linear combination functions
* Matrices of arbitrary size

I hope that you were as surprised (and pleased) as I was.
