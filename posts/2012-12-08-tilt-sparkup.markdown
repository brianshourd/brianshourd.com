---
title: Thing I Learned Today: Sparkup
author: Brian Shourd
date: December 8, 2012
tags: coding
---

Working on this website has me editing and writing a huge (actually
tiny) amount of html. It's really really annoying.

But since I use Vim, I know that there has to be some kind of magic
plugin that makes writing html that much easier. Today I found it, and
it's called [Sparkup](https://github.com/rstacruz/sparkup).

Basically, its designed to allow you to add html elements with a minimum
number of keystrokes, using css-style syntax to keep the cognitive load
down. It's best illustrated with an example. Suppose that I want to
insert a new `div` element. In insert mode, I just type out `div`, then
press `<c-e>`, and sparkup automatically replaces it with

    <div></div>

It also conveniently places my cursor exactly where you would expect,
and keeps me in insert mode.

But I never want a classless `div`. I want a `div` with
`id="navigation"` and `class="nav-right"`. Then type

    div.nav-right#navigation

and `<c-e>` expands it to

    <div class='nav-right' id='navigation'></div>

once again with the cursor in exactly the proper location. Notice that
the syntax used was just css, and the plugin allows spaces, if you so
desire. How about nested elements?

    ul > li > a[href=http://brianshourd.com]{Home}

becomes

    <ul>
        <li>
            <a href='http://brianshourd.com'>Home</a>
        </li>
    </ul>

and

    ul > li > a*4

becomes

    <ul>
        <li>
            <a href=''></a>
            <a href=''></a>
            <a href=''></a>
            <a href=''></a>
        </li>
    </ul>

with your cursor in the first `a` tag. Pressing `<c-n>` will move your
cursor to the next logical break, for quick filling in of things like
lists. There's plenty more that it can do, too, this is just a sample,
but I like it because it makes creating html boilerplate so very fast 
and easy. If you use vim (or textmate), you really should check it out.

P.S. If you use vim and [Vundle](https://github.com/gmarik/vundle)
(which I recommend), then there is an [alternate
repository](https://github.com/tristen/vim-sparkup) which bundles just
the vim part for easy installation. It's actually supposed to work for
pathogen, but it works for vundle too.
