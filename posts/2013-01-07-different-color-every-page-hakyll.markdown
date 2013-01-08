---
title: A Different Color Scheme for Every Page With Hakyll
author: Brian Shourd
date: January 7, 2013
tags: coding, haskell, hakyll
---

As I mess more and more with my website, I find that I'm having trouble
deciding on a color scheme. I like mostly black and white, with a single
accent color on which all else is built (including shades of that
color).

Yeah, so I'm not a designer.

Anyway, once I had settled (more or less) on a theming, I couldn't
decide *which* color to choose. They are all so good. So I decided to
challenge myself to see if I could just use them all. Every page, use a
different base color, presumably based on some kind of hash of the page.

After a while, I came up with a solution. I'm already using
[Sass](http://www.sass-lang.com), so I created a stylesheet that encodes
all of my color information.

~~~{.css}
/* colorize.scss */

$basecolor: $$color$$;
$bgcolorpre: lighten($basecolor, 30%);
$bgcolorout: darken($basecolor, 60%);
$linkcolor: $basecolor;

a {
    &:link {color:$linkcolor};
    &:visited {color: darken($linkcolor, 10%)};
    &:hover {color: lighten($linkcolor, 15%)};
    &:active {color: lighten($linkcolor, 10%)};
}

html, body {
    background-color: $bgcolorout;
}

div#banner {
    img {
        background-color: $basecolor;
    }
}

pre {
    background-color: $bgcolorpre;
}
~~~

You can see that it is a Hakyll template with only a single piece of
metadata: `$$color$$`.

Now, how to attach this to every single page? Since every page will need
a different rendered version of this stylesheet, it makes perfect sense
just to write it into the head of each html file. So I added this line
to my `default.html` template:

~~~{.html}
<!-- default.html -->
...
<head>
    ...
    <style type="text/css">$$colorize$$</style>
</head>
...
~~~

Now, all that remains is to include a compiler for this information in
`hakyll.hs`. This actually took me quite a while, due to a combination
of my unfamiliarity with Hakyll, my unfamiliarity with Arrows, and my
unfamiliarity with Haskell in general. But I'm so glad I did, because
it helped with my understanding of all three tremendously.

Here's the relevant code:

~~~{.haskell}
-- hakyll.hs
...
-- Fill in the $$css$$ option with colors!
colorizePage :: Compiler (Page String) (Page String)
colorizePage = requireA "templates/colorize.scss" $
    arr (\(p, t) -> (p,pageFromTemplate t $ getField "url" p))
        >>> second (pageBody ^>> sassifyString)
        >>> arr (uncurry . flip $ setField "colorize")
        where
            pageFromTemplate t s = applyTemplate t $ fromMap . fromList $ [("color", hashColor s)]

-- Run sass, then compress
sassifyString :: Compiler String String
sassifyString = unixFilter "sass" ["-s", "--scss", "--trace", "--load-path", "css"]
    >>> arr compressCss

-- Take in a string (url is used), and perform a
-- simple hash to get a valid hsl color for sass
hashColor :: String -> String
hashColor s = "hsl(" ++ hash ++ ", 39, 60)" where
    hash = show $ (flip mod) 360 $ sum . map ord $ s
...
~~~

It's only 9 lines (excluding comments and type signatures), but I had
the hardest time figuring them out. Let me explain how they work.

Firstly, I knew that I wanted a `Compiler (Page String) (Page String)`,
so that I could just interject it into each page's compilation arrow.
That's the type signature that makes sense - we want to take a page, and
add the `$$colorize$$` metadata for when we apply the `default.html`
template. To do that, we'll need to pull in the `colorize.scss`
template, which means using one of `require` or `requireA`, both Hakyll
functions. I used `requireA` since I'm constructing a `Compiler`. The
relevant type signature is

    requireA "templates/colorize.scss" 
        :: Compiler (Page String, Template) (Page String)
        -> Compiler (Page String) (Page String)

Now we need to supply a `Compiler (Page String, Template) (Page
String)`. Remember that a `Compiler` is an `Arrow`, so essentially an
abstraction of a function `(Page String, Template) -> (Page String)`.
We'll do this in several steps:

1. Turn the template (`colorize.scss`) into a page with the proper `$$color$$`
   field, which we get from the first page.

        arr (\(p, t) -> (p,pageFromTemplate t $ getField "url" p))
        :: Compiler (Page String, Template) (Page String, Page String)

2. Run the filled-in template through `sass` to get a plain string of
   css

        >>> second (pageBody ^>> sassifyString)
        :: Compiler (Page String, Page String) (Page String, String)

3. Take the resulting string and plug it into the page as the
   `$$colorize$$` field.

        >>> arr (uncurry . flip $ setField "colorize")
        :: Compiler (Page String, String) (Page String)

The helper functions just assist. The `sassifyString` function runs a
string through sass, then compresses the resulting css. The
`pageFromTemplate` function just creates a blank page from a template
with the `$$css$$` field set. The `hashColor` function is a very basic
hash on the `$$url$$` field of the page, giving us a color in hsl format
with hashed hue, and fixed saturation and luminosity (so we don't get
colors that are too dark or light).

That's all there is to it! I can just throw this compiler into the
compilation toolchain for all of my pages and I get specialized
individual css colors for every page.

I had a ton of fun writing this code, even though the result is
basically not noticeable (or really desirable - it's kind of jarring to
have the colors change all the time). Send me an email if you have
questions about the code, I would love to chat about it.
