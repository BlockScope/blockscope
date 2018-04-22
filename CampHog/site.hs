{-# LANGUAGE OverloadedStrings #-}

import Control.Applicative ((<$>))
import Data.Monoid ((<>))
import Hakyll
    ( Rules, Pattern, Context(..), Compiler, Item(..), Configuration(..)
    , defaultContext
    , dateField
    , constField
    , listField
    , loadAll
    , loadBody
    , loadAndApplyTemplate
    , applyTemplateList
    , applyAsTemplate
    , templateCompiler
    , copyFileCompiler
    , match
    , compile
    , getResourceBody
    , relativizeUrls
    , route
    , idRoute
    , recentFirst
    , create
    , makeItem
    , setExtension
    , fromList
    , fromGlob
    , pandocCompilerWith
    , hakyllWith
    , defaultHakyllReaderOptions
    , defaultHakyllWriterOptions
    , defaultConfiguration
    )
import Text.Pandoc.Options
    (ReaderOptions(..), WriterOptions(..), HTMLMathMethod(..))

pandocReaderOptions :: ReaderOptions
pandocReaderOptions = defaultHakyllReaderOptions

pandocWriterOptions :: WriterOptions
pandocWriterOptions =
    defaultHakyllWriterOptions
        { writerHTMLMathMethod = MathJax ""
        }

static :: Pattern -> Rules ()
static f = match f $ do
    route idRoute
    compile copyFileCompiler

directory :: (Pattern -> Rules a) -> String -> Rules a
directory act f = act $ fromGlob $ f ++ "/**"

config :: Configuration
config =
    defaultConfiguration
        { providerDirectory = "CampHog"
        }

main :: IO ()
main = hakyllWith config $ do
    {-- SEE: http://vapaus.org/text/hakyll-configuration.html --}
    mapM_ (directory static) ["css", "font", "js", "images"]

    match (fromList ["about.markdown", "contact.markdown"]) $ do
        route $ setExtension "html"
        compile $ pandocCompilerWith pandocReaderOptions pandocWriterOptions
            >>= loadAndApplyTemplate "templates/default.html" defaultContext
            >>= relativizeUrls

    match "posts/*" $ do
        route $ setExtension "html"
        compile $ pandocCompilerWith pandocReaderOptions pandocWriterOptions
            >>= loadAndApplyTemplate "templates/post.html" postCtx
            >>= loadAndApplyTemplate "templates/default.html" postCtx
            >>= relativizeUrls

    create ["archive.html"] $ do
        route idRoute
        compile $ do
            posts <- loadAll "posts/*" >>= recentFirst

            let archiveCtx =
                    listField "posts" postCtx (return posts)
                    <> constField "title" "Archives"
                    <> defaultContext

            makeItem ""
                >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
                >>= loadAndApplyTemplate "templates/default.html" archiveCtx
                >>= relativizeUrls

    match "index.html" $ do
        route idRoute
        compile $ do
            posts <- loadAll "posts/*" >>= recentFirst

            let indexCtx =
                    listField "posts" postCtx (return posts)
                    <> constField "title" ""
                    <> defaultContext

            getResourceBody
                >>= applyAsTemplate indexCtx
                >>= loadAndApplyTemplate "templates/default.html" postCtx
                >>= relativizeUrls

    match "templates/*" $ compile templateCompiler

postCtx :: Context String
postCtx = dateField "date" "%Y-%m-%d" <> defaultContext

postList :: ([Item String] -> [Item String]) -> Compiler String
postList sortFilter = do
    posts <- sortFilter <$> loadAll "posts/*"
    itemTpl <- loadBody "templates/post-item.html"
    list <- applyTemplateList itemTpl postCtx posts
    return list
