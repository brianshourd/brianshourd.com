---
title: Thing I Learned Today: Website Generation With Hakyll
author: Brian Shourd
date: December 2, 2012
tags: coding, haskell
---

Here's my current blogging setup: I type basically all my posts in [Markdown](http://daringfireball.net/projects/markdown/) using vim through SSH on an iPad with an external keyboard and the [VimRepress plugin](http://www.vim.org/scripts/script.php?script_id=3510) to post those ramblings to Wordpress. When I stop to think about it, it's actually pretty convoluted.

I *could* use the built-in app for Wordpress, of course, but it doens't have support for markdown. I *could* use a third-party app to write in markdown, but then I wouldn't be able to use vim (one of the joys of my life is vim+tmux). When I stop to think, the things that I really want to do are:

1. Publish simple blog posts
2. written in markdown
3. using vim
4. with LaTeX support
5. through SSH

Since Wordpress doesn't officially support 2, 3, or 5, maybe it isn't really the best choice for me. I looked it up, and there is a [whole class of programs](http://nanoc.stoneship.org/docs/1-introduction/#similar-projects) called *static site generators*. I need to be doing this. Now.

After a painstaking process of trying to choose one, I realized that I care much less about features than about my ability to hack it, so I picked [Hakyll](http://jaspervdj.be/hakyll/) as part of my effort to learn Haskell. It looks like it does everything that I want it to with just a few simple lines.

Now I just have to get it set up. Here's the plan:

Filesystem:

    /brianshourd.com/
      |--posts/
      |--drafts/
      |--templates/
      |--css/
      |--images/
      \--pages/

I can just put, in markdown, a single file for each draft under `/drats/`. When it's finished, I move it to `/posts/`, then fire up `hakyll` to compile all the posts and pages using the templates, css, and images. It puts them in their very own new directory, which I can just rsync to my web host.

This fits my workflow much better, so to anyone who might view this - expect a site migration soon. By the time you read this, I'll hopefully have already moved to a hakyll-based blog. Or maybe I'll have moved on even more past that by then.
