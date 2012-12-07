{-# LANGUAGE OverloadedStrings #-}
module Main where

import Prelude hiding (id)
import Control.Category (id)
import Control.Arrow ((>>>), (***), arr)
import Data.Monoid (mempty, mconcat)
import Text.Pandoc (WriterOptions (..), HTMLMathMethod (MathJax))

import Hakyll

main :: IO ()
main = hakyll $ do
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
            >>> unixFilter "sass" ["-s", "--scss", "--trace", "--load-path", "css"]
            >>> arr compressCss 

    -- Render posts
    match "posts/*" $ do
        route   $ setExtension ".html"
        compile $ pageCompilerWith defaultHakyllParserState pandocOptions
            >>> arr (checkMathOption)
            >>> arr (\p -> setField "description" (getField "body" p) p)
            >>> applyTemplateCompiler "templates/post.html"
            >>> arr (setField "siteTitle" "Blog")
            >>> applyTemplateCompiler "templates/default.html"
            >>> relativizeUrlsCompiler

    -- Render posts list
    match  "posts.html" $ route idRoute
    create "posts.html" $ constA mempty
        >>> arr (checkMathOption)
        >>> arr (setField "siteTitle" "All Posts")
        >>> requireAllA "posts/*" addPostList
        >>> applyTemplateCompiler "templates/posts.html"
        >>> applyTemplateCompiler "templates/default.html"
        >>> relativizeUrlsCompiler

    -- Index
    match  "index.html" $ route idRoute
    create "index.html" $ constA mempty
        >>> arr (checkMathOption)
        >>> arr (setField "siteTitle" "Home")
        >>> requireAllA "posts/*" (id *** arr (take 3 . reverse . chronological) >>> addPostList)
        >>> applyTemplateCompiler "templates/index.html"
        >>> applyTemplateCompiler "templates/default.html"
        >>> relativizeUrlsCompiler

    -- About
    match "about.markdown" $ do
        route   $ setExtension ".html"
        compile $ pageCompiler
            >>> arr (checkMathOption)
            >>> arr (setField "siteTitle" "About")
            >>> applyTemplateCompiler "templates/default.html"
            >>> relativizeUrlsCompiler    
            
    -- Render RSS feed
--    match  "rss.xml" $ route idRoute
--    create "rss.xml" $
--        requireAll_ "posts/*" 
--        >>> renderRss feedConfiguration

    -- Read templates
    match "templates/*" $ compile templateCompiler

-- | Auxiliary compiler: generate a post list from a list of given posts, and
-- add it to the current page under @$posts@
--
addPostList :: Compiler (Page String, [Page String]) (Page String)
addPostList = setFieldA "posts" $
    arr (reverse . chronological)
        >>> require "templates/postitem.html" (\p t -> map (applyTemplate t) p)
        >>> arr mconcat
        >>> arr pageBody

--feedConfiguration :: FeedConfiguration
--feedConfiguration = FeedConfiguration
--    { feedTitle       = "Blog - Brian Shourd"
--    , feedDescription = "Thing I Learned Today"
--    , feedAuthorName  = "Brian Shourd"
--    , feedAuthorEmail = "brian.shourd@gmail.com"
--    , feedRoot        = "http://shou4577.uk.to:8000"
--    }

pandocOptions :: WriterOptions
pandocOptions = defaultHakyllWriterOptions
    { writerHTMLMathMethod = MathJax ""
    }

checkMathOption :: Page String -> Page String
checkMathOption page =
    case getFieldMaybe "math" page of
        Nothing -> setField "mathjax" "" page
        Just _  -> setField "mathjax" "<script type=\"text/javascript\" src=\"http://cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-AMS-MML_HTMLorMML\" />" page

