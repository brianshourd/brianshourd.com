---
title: Xmonad Sub-Fullscreen Layout
author: Brian Shourd
date: April 15, 2013
tags: coding, haskell
---

*Edit:* I've slightly updated this code and [put it on
github](http://github.com/brianshourd/xmonad-layout-padding).

I recently got a new Macbook Air, thus joining the ranks of people who
own laptops (previously, I was using an iPad with external keyboard and
doing all my work in iSSH on a home server). Naturally, the first thing
that I did was set up linux on it. I'm currently dual-booting, since I
think that I'd like to try my hand at iOS programming, but so far
[Xubuntu](http://xubuntu.org) is working great. (Side note: for the past
several years I've been using [Arch](http://archlinux.org), and I've been very happy with it,
but I wanted it to 'just work' on my MBA, so I went with a \*buntu
variant. No complaints so far!)

The first thing that I did was get the [xmonad window
manager](http://xmonad.org) running in Xfce [using this
guide](http://www.haskell.org/haskellwiki/Xmonad/Using_xmonad_in_XFCE).
I like Xfce because it is lightweight, but it keeps enough stuff around
to be convenient (like a tray for icons, a window bar, and a menu for
when I need it or somebody who doesn't know all my keyboard shortcuts is
using my computer). I *love* xmonad because it means that I never have
to use a mouse.

Maybe it's just me, but I never really got the hang of the trackpad. I
like the MBA's trackpad, but I'm still faster with the keyboard. xmonad
let's me completely manage my windows without ever touching the mouse,
and that alone is reason to use it.

Actually, that's reason to use a tiling window manager. But xmonad is
just one of them (other popular choices include
[awesome](http://awesome.naquadah.org), [dwm](http://dwm.suckless.org),
[qtile](http://qtile.org), and [wmii](http://wmii.googlecode.com)). The
reason that I picked xmonad is because it is written and configured in
haskell, and I like haskell. Honestly, I didn't even compare features or
try the other ones - I'm sure that I would like them too. But I have
absolutely no complaints about xmonad.

Except one.

For a lot of my activities, I actually only have one window up.
Especially web browsing. And maybe I'm just used to smaller screens,
but fullscreen on the 13" MBA is too big for a lot of websites. Under
most window managers, I would just shrink the window so that it didn't
take up the whole screen - but such doesn't work with tiling window
managers.

That's why I created a new layout, re-using the code from the
[`Xmonad.Layout.Spacing`
module](http://xmonad.org/xmonad-docs/xmonad-contrib/XMonad-Layout-Spacing.html).
Here's the code for the `SideSpacing` module:

~~~{.haskell}
-- File: SideSpacing.hs
-- Based on Xmonad.Layout.Spacing created by Brent Yorgey
{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}

module SideSpacing (sideSpacing) where

import Graphics.X11 (Rectangle(..))
import Control.Arrow (second)
import XMonad.Util.Font (fi)

import XMonad.Layout.LayoutModifier

-- | Surround all windows by a certain number of pixels of blank space.
sideSpacing :: Int -> l a -> ModifiedLayout SideSpacing l a
sideSpacing p = ModifiedLayout (SideSpacing p)

data SideSpacing a = SideSpacing Int deriving (Show, Read)

instance LayoutModifier SideSpacing a where

    pureModifier (SideSpacing p) _ _ wrs = (map (second $ shrinkRect p) wrs, Nothing)

    modifierDescription (SideSpacing p) = "Side Spacing " ++ show p

shrinkRect :: Int -> Rectangle -> Rectangle
shrinkRect p (Rectangle x y w h) = Rectangle (x+fi p) (y) (w-2 * fi p) (h)
~~~

It barely differs at all from Brent Yorgey's orginal module, simply
changing some names, the last line, and removing some unneeded
functionality. Now, to use it, we simply change the `layoutHook` to
include a new layout, which I called `centered`. Here is my `xmonad.hs`
file, for context. 

~~~
-- File: xmonad.hs
import XMonad
import XMonad.Config.Desktop
import XMonad.Config.Xfce
import XMonad.Hooks.ManageDocks
import XMonad.Layout.Grid
import XMonad.Layout.Spacing
import XMonad.Layout.NoBorders

import SideSpacing

import qualified Data.Map as M

main = xmonad $ xfceConfig
	{ focusedBorderColor = "#000000"
	, normalBorderColor = "#eee8d5"
	, borderWidth = 4
	, layoutHook = avoidStruts $ myLayoutHook
	, keys = myKeys <+> keys desktopConfig
	}


myLayoutHook = centered ||| tiled ||| full ||| Grid
    where  
        centered = sideSpacing 175 $ Full

        tiled = spacing 2 $ Tall nmaster delta ratio  

        full = spacing 0 $ noBorders Full

        -- The default number of windows in the master pane  
        nmaster = 1  

        -- Default proportion of screen occupied by master pane  
        ratio = 5/8  

        -- Percent of screen to increment by when resizing panes  
        delta = 5/100  

myKeys (XConfig {modMask = modm}) = M.fromList $
	[ ((modm,               xK_p     ), spawn "dmenu_run")
    , ((modm .|. shiftMask, xK_h     ), spawn "thunar /home/brian/")
	, ((modm .|. shiftMask, xK_Return), spawn "urxvt")
	, ((modm .|. shiftMask, xK_p     ), spawn "xfce4-appfinder")
	, ((modm .|. shiftMask, xK_q     ), spawn "xfce4-session-logout")
	]
~~~

That's it. The result looks like

![Centered Xmonad Layout 1](/images/xmonadScreenshot1.jpg)

![Centered Xmonad Layout 2](/images/xmonadScreenshot2.jpg)

It works very well for me, and if I have multiple windows open, I just
press `<Alt><Space>` to switch to a standard tiled layout.

