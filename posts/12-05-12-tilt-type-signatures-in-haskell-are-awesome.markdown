---
title: Thing I Learned Today: Type Signatures in Haskell are Awesome
author: Brian Shourd
date: December 5, 2012
tags: coding, haskell
---

My last post was about getting [Hakyll](http://www.jaspervdj.be/hakyll/)
set up to take over my blog, instead of wordpress. Good news - it's
done! At least, it's working. I'm still using some example templates and
css, which I'd like to swap out for some of my own designs.

One of the things that I need on my blog is a nice way to typeset
mathematics. Since Hakyll is built on Pandoc, it can handle LaTeX
conversions in a lot of ways. My personal choice is
[Mathjax](http://www.mathjax.org/), which looks awesome but has the
drawback that it runs a fair chuck of javascript in the background.

The only thing left to do was work out exactly how I could get Hakyll to
tell Pandoc to use the built-in Mathjax option. In addition, it would be
nice if it only did it for pages that actually need it, so that I'm not
making all my readers download a chunk of JS just to view a mathless
static webpage.

A quick search revealed [John Lenz's
blog](http://blog.wuzzeb.org/posts/2012-06-08-hakyll-and-latex.html),
with a very helpful guide on doing exactly what I just said. It even has
explicit code snippets accompanying it. Only problem - he didn't say how
to integrate his code snippets.

It turns out that this isn't a problem at all. He basically included two
code snippets:

~~~{.haskell}
pandocOptions :: WriterOptions
pandocOptions = defaultHakyllWriterOptions
    { writerHTMLMathMethod = MathJax ""
    }
~~~

~~~{.haskell}
checkMathOption :: Page String -> Page String
checkMathOption page =
  case getFieldMaybe "math" page of
     Nothing -> setField "mathjax" "" page
     Just _  -> setField "mathjax" "<script type=\"text/javascript\" src=\"http://cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-AMS-MML_HTMLorMML\" />" page
~~~

The type signatures immediately told me how to use the code. For the
first, I needed to do a quick documentation search for the
`WriterOptions` type in the Hakyll docs. It turned up a few functions,
specifically the `pageCompilerWith` function, which is exactly what I
needed. I just modified my page compiler:

~~~{.haskell}
    match "posts/*" $ do
        route   $ setExtension ".html"
-       compile $ pageCompiler
+       compile $ pageCompilerWith defaultHakyllParserState pandocOptions
            >>> applyTemplateCompiler "templates/post.html"
            >>> applyTemplateCompiler "templates/default.html"
            >>> relativizeUrlsCompiler
~~~

For the second, I didn't even need to consult the docs (but only because
I had already glanced through them). Since it takes a `Page` to a
`Page`, it's the precursor to an arrow applied to a compiled page. I
needed to make it an `Arrow`, then stick it in the compilation chain.


~~~{.haskell}
    match "posts/*" $ do
        route   $ setExtension ".html"
        compile $ pageCompilerWith defaultHakyllParserState pandocOptions
+           >>> arr (checkMathOption)
            >>> applyTemplateCompiler "templates/post.html"
            >>> applyTemplateCompiler "templates/default.html"
            >>> relativizeUrlsCompiler
~~~

How awesome was that? Because of the strong hints given by the type
signatures, I didn't even have to think at all!

Now of course, Haskell type signatures are great for lots of reasons,
not just because it makes copying random code off the internet easier
(in fact, lots of people might say that's a *bad* thing), but I got a
kick out of how this made me feel like an expert even though I'm still
just learning.
