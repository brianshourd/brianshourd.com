{-# LANGUAGE OverloadedStrings #-}
module Main where

import Prelude hiding (id)
import Control.Category (id)
import Control.Applicative ((<$>))
import Control.Arrow ((>>>), (***), (^>>), arr, second)
import Data.Maybe (fromMaybe)
import Data.Monoid (mempty, mconcat, (<>))
import Data.Map (fromList, lookup)
import Data.Char (ord)
import Text.Pandoc (WriterOptions (..), HTMLMathMethod (MathJax))

import Hakyll

config :: Configuration
config = defaultConfiguration {
    deployCommand = "rsync -avcz _site/ briansho@brianshourd.com:public_html/"
}

main :: IO ()
main = hakyllWith config $ do
    -- Move images
    match "images/*" $ do
        route   idRoute
        compile copyFileCompiler

    -- Compress CSS
    match "css/style.scss" $ do
        route   $ setExtension ".css"
        compile $ getResourceString
            >>= sassify

    -- Buid Tags
    tags <- buildTags "posts/*" (fromCapture "tags/*.html")

    -- Render posts
    match "posts/*" $ do
        route   $ setExtension ".html"
        compile $ do
            pandocCompilerWith defaultHakyllReaderOptions pandocOptions
            >>= saveSnapshot "content"
            >>= loadAndApplyTemplate "templates/post.html"
                    (postCtx tags <> defaultContext)
            >>= loadAndApplyTemplate "templates/default.html" 
                    (constField "siteTitle" "Blog" <> topCtx 
                        <> defaultContext)
            >>= relativizeUrls

    -- Render posts page
    create ["posts.html"] $ do
        route idRoute
        compile $ do
            list <- postList tags "posts/*" recentFirst
            makeItem ""
                >>= loadAndApplyTemplate "templates/posts.html"
                        (constField "posts" list <>
                            defaultContext)
                >>= loadAndApplyTemplate "templates/default.html"
                        (constField "siteTitle" "All Posts" 
                            <> topCtx <> defaultContext)
                >>= relativizeUrls

    -- Index
    create ["index.html"] $ do
        route idRoute
        compile $ do
            list <- postList tags "posts/*" $ take 5 . recentFirst
            makeItem ""
                >>= loadAndApplyTemplate "templates/index.html" 
                        (constField "posts" list <> defaultContext)
                >>= loadAndApplyTemplate "templates/default.html"
                        (constField "siteTitle" "Home" <> topCtx 
                            <> defaultContext)
                >>= relativizeUrls

    -- Create pages for tags, copied from jaspervd
    tagsRules tags $ \tag pattern -> do
        let title = "Tag: " ++ tag
        -- Copied from posts, need to refactor
        route idRoute
        compile $ do
            list <- postList tags pattern recentFirst
            makeItem ""
                >>= loadAndApplyTemplate "templates/posts.html"
                        (constField "posts" list <> defaultContext)
                >>= loadAndApplyTemplate "templates/default.html" 
                        (constField "siteTitle" title <> topCtx 
                            <> defaultContext)
                >>= relativizeUrls

    -- About
    match "about.markdown" $ do
        route   $ setExtension ".html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/default.html" 
                    (constField "siteTitle" "About" <> topCtx 
                        <> defaultContext)
            >>= relativizeUrls
    
    -- Rss Feed
    create ["rss.xml"] $ do
        route idRoute
        compile $ do
            loadAllSnapshots "posts/*" "content"
                >>= return . take 10 . recentFirst
                >>= renderAtom feedConfiguration 
                        (bodyField "description" <> defaultContext)

    -- Read templates
    match "templates/*" $ compile templateCompiler

feedConfiguration :: FeedConfiguration
feedConfiguration = FeedConfiguration
    { feedTitle       = "Blog - Brian Shourd"
    , feedDescription = "Thing I Learned Today"
    , feedAuthorName  = "Brian Shourd"
    , feedAuthorEmail = "brian.shourd@gmail.com"
    , feedRoot        = "http://brianshourd.com/"
    }

pandocOptions :: WriterOptions
pandocOptions = defaultHakyllWriterOptions
    { writerHTMLMathMethod = MathJax ""
    }

postCtx :: Tags -> Context String
postCtx tags = mconcat
    [ dateField "date" "%B %e, %Y"
    , tagsField "tags" tags
    , defaultContext
    ]

topCtx :: Context String
topCtx = mconcat
    [ field "mathjax" mathjax
    , field "colorize" colorize
    ]

postList :: Tags -> Pattern -> ([Item String] -> [Item String]) -> Compiler String
postList tags pattern preprocess' = do
    postItemTpl <- loadBody "templates/postitem.html"
    posts       <- preprocess' <$> loadAll pattern
    applyTemplateList postItemTpl (postCtx tags) posts

-- Take in a string and perform a simple hash to get a valid hsl color
-- for sass
hashColor :: String -> String
hashColor s = "hsl(" ++ hash ++ ", 39, 60)" where
    hash = show . (flip mod) 360 . sum . map ord $ s

-- Run sass, then compress
sassify item = withItemBody (unixFilter "sass" ["-s", "--scss", "--trace", "--load-path", "css"]) item
    >>= return . fmap compressCss

-- Add css with custom colors based on a hash of the page contents
colorize :: Item String -> Compiler String
colorize item = do
    let color = hashColor . itemBody $ item
    cssItem <- makeItem "" 
        >>= loadAndApplyTemplate "templates/colorize.scss" (constField "color" color)
        >>= sassify
    return $ itemBody cssItem

-- Only load mathjax if there is actually math on the page, indicated in
-- the metadata
mathjax :: Item String -> Compiler String
mathjax item = do
    metadata <- getMetadata (itemIdentifier item)
    return $ case Data.Map.lookup "math" metadata of
        Nothing -> ""
        Just _  -> "<script type=\"text/javascript\" src=\"http://cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-AMS-MML_HTMLorMML\" />"
