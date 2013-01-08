{-# LANGUAGE OverloadedStrings #-}
module Main where

import Prelude hiding (id)
import Control.Category (id)
import Control.Arrow ((>>>), (***), (^>>), arr, second)
import Data.Monoid (mempty, mconcat)
import Data.Map (fromList)
import Data.Char (ord)
import Text.Pandoc (WriterOptions (..), HTMLMathMethod (MathJax))

import Hakyll

config :: HakyllConfiguration
config = defaultHakyllConfiguration {
    deployCommand = "rsync -avcz _site/ briansho@brianshourd.com:public_html/"
}

main :: IO ()
main = hakyllWith config $ do
    -- Move images
    match "images/*" $ do
        route   idRoute
        compile copyFileCompiler

    -- Compress CSS
--    match "css/*" $ do
--        route   idRoute
--        compile compressCssCompiler
    match "css/style.scss" $ do
        route   $ setExtension ".css"
        compile $ getResourceString 
            >>> sassifyString

    -- Render posts
    match "posts/*" $ do
        route   $ setExtension ".html"
        compile $ pageCompilerWith defaultHakyllParserState pandocOptions
            -- To load MathJax javascript or not
            >>> arr checkMathOption
            -- for RSS feed
            >>> arr (copyBodyToField "description")
            >>> colorizePage
            >>> applyTemplateCompiler "templates/post.html"
            >>> arr (setField "siteTitle" "Blog")
            >>> applyTemplateCompiler "templates/default.html"
            >>> relativizeUrlsCompiler

    -- Render posts list
    match  "posts.html" $ route idRoute
    create "posts.html" $ constA mempty
        -- To load MathJax javascript or not
        >>> arr checkMathOption
        >>> colorizePage
        >>> arr (setField "siteTitle" "All Posts")
        >>> requireAllA "posts/*" addPostList
        >>> applyTemplateCompiler "templates/posts.html"
        >>> applyTemplateCompiler "templates/default.html"
        >>> relativizeUrlsCompiler

    -- Index
    match  "index.html" $ route idRoute
    create "index.html" $ constA mempty
        -- To load MathJax javascript or not
        >>> arr checkMathOption
        >>> colorizePage
        >>> arr (setField "siteTitle" "Home")
        >>> requireAllA "posts/*" (id *** arr (take 3 . reverse . chronological) >>> addPostList)
        >>> applyTemplateCompiler "templates/index.html"
        >>> applyTemplateCompiler "templates/default.html"
        >>> relativizeUrlsCompiler

    -- About
    match "about.markdown" $ do
        route   $ setExtension ".html"
        compile $ pageCompiler
            -- To load MathJax javascript or not
            >>> arr checkMathOption
            >>> colorizePage
            >>> arr (setField "siteTitle" "About")
            >>> applyTemplateCompiler "templates/default.html"
            >>> relativizeUrlsCompiler
            
    -- Render RSS feed
    match  "rss.xml" $ route idRoute
    create "rss.xml" $
        requireAll_ "posts/*" 
        >>> renderRss feedConfiguration

    -- Read templates
    match "templates/*" $ compile templateCompiler

feedConfiguration :: FeedConfiguration
feedConfiguration = FeedConfiguration
    { feedTitle       = "Blog - Brian Shourd"
    , feedDescription = "Thing I Learned Today"
    , feedAuthorName  = "Brian Shourd"
    , feedAuthorEmail = "brian.shourd@gmail.com"
    , feedRoot        = "http://www.brianshourd.com/rss.xml"
    }

pandocOptions :: WriterOptions
pandocOptions = defaultHakyllWriterOptions
    { writerHTMLMathMethod = MathJax ""
    }

-- | Auxiliary compiler: generate a post list from a list of given posts, and
-- add it to the current page under @$posts@
--
addPostList :: Compiler (Page String, [Page String]) (Page String)
addPostList = setFieldA "posts" $
    arr (reverse . chronological)
        >>> require "templates/postitem.html" (\p t -> map (applyTemplate t) p)
        >>> arr mconcat
        >>> arr pageBody

-- Take in a string (url is used), and perform a
-- simple hash to get a valid hsl color for sass
hashColor :: String -> String
hashColor s = "hsl(" ++ hash ++ ", 39, 60)" where
    hash = show $ (flip mod) 360 $ sum . map ord $ s

-- Run sass, then compress
sassifyString :: Compiler String String
sassifyString = unixFilter "sass" ["-s", "--scss", "--trace", "--load-path", "css"]
    >>> arr compressCss

-- Fill in the $css$ option with colors!
colorizePage :: Compiler (Page String) (Page String)
colorizePage = requireA "templates/colorize.scss" $
    arr (\(p, t) -> (p,pageFromTemplate t $ getField "url" p))
        >>> second (pageBody ^>> sassifyString)
        >>> arr (uncurry . flip $ setField "colorize")
        where
            pageFromTemplate t s = applyTemplate t $ fromMap . fromList $ [("color", hashColor s)]

checkMathOption :: Page String -> Page String
checkMathOption page =
    case getFieldMaybe "math" page of
        Nothing -> setField "mathjax" "" page
        Just _  -> setField "mathjax" "<script type=\"text/javascript\" src=\"http://cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-AMS-MML_HTMLorMML\" />" page

