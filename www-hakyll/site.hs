{-# LANGUAGE OverloadedStrings #-}

import Control.Applicative (empty)
import qualified Data.Map as Map
import Data.List (intersperse)
import Hakyll
    ( Context(..), Item(..), Configuration(..)
    , Rules, Pattern, Compiler, Identifier
    , defaultContext
    , dateField
    , tagsFieldWith
    , constField
    , listField
    , loadAll
    , loadAndApplyTemplate
    , templateCompiler
    , copyFileCompiler
    , match
    , compile
    , relativizeUrls
    , route
    , idRoute
    , customRoute
    , recentFirst
    , create
    , makeItem
    , setExtension
    , fromFilePath
    , toFilePath
    , fromGlob
    , fromCapture
    , pandocCompilerWith
    , hakyllWith
    , defaultHakyllReaderOptions
    , defaultHakyllWriterOptions
    , defaultConfiguration
    , toUrl
    , gsubRoute
    , composeRoutes
    )
import Hakyll.Web.Tags (Tags, buildTags, tagsRules, getTags)
import Text.Pandoc.Options (ReaderOptions(..), WriterOptions(..), HTMLMathMethod(..))
import Skylighting (Syntax, parseSyntaxDefinition)
import Text.Blaze.Html ((!), toHtml, toValue)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import System.FilePath

pandocReaderOptions :: Maybe Syntax -> ReaderOptions
pandocReaderOptions Nothing = defaultHakyllReaderOptions
pandocReaderOptions (Just _) =
        defaultHakyllReaderOptions { readerIndentedCodeClasses = ["smt2"] }

pandocWriterOptions :: Maybe Syntax -> WriterOptions
pandocWriterOptions Nothing = defaultHakyllWriterOptions { writerHTMLMathMethod = MathJax "" }
pandocWriterOptions (Just syntax) =
        defaultHakyllWriterOptions
            { writerHTMLMathMethod = MathJax ""
            , writerSyntaxMap = syntaxMap
            }
        where
            syntaxMap =
                Map.insert "smt2" syntax
                $ writerSyntaxMap defaultHakyllWriterOptions

pandoc :: Maybe Syntax -> Compiler (Item String)
pandoc syntax =
    pandocCompilerWith (pandocReaderOptions syntax) (pandocWriterOptions syntax)

static :: Pattern -> Rules ()
static f = match f $ do
    route idRoute
    compile copyFileCompiler

directory :: (Pattern -> Rules a) -> String -> Rules a
directory act f = act $ fromGlob $ f ++ "/**"

config :: Configuration
config = defaultConfiguration { providerDirectory = "www-hakyll" }

mkStatic :: FilePath -> Rules ()
mkStatic path = do
    -- SEE: https://aherrmann.github.io/programming/2016/01/31/jekyll-style-urls-with-hakyll/
    route
        $ gsubRoute "static/" (const "")
        `composeRoutes`
        customRoute ((</> "index.html") . fst . splitExtension . toFilePath)

    compile $ do
        pandoc Nothing
            >>= loadAndApplyTemplate "templates/about.html" postCtx
            >>= loadAndApplyTemplate (fromFilePath $ "templates" </> path <.> ".html") postCtx
            >>= relativizeUrls

pandocTemplate :: Identifier -> Context String -> Maybe Syntax -> Compiler (Item String)
pandocTemplate template ctx syntax = do
    pandoc syntax
        >>= loadAndApplyTemplate template ctx
        >>= loadAndApplyTemplate "templates/default.html" ctx
        >>= relativizeUrls

pandocPost :: Context String -> Maybe Syntax -> Compiler (Item String)
pandocPost = pandocTemplate "templates/post.html"

mkPostsCtx :: String -> String -> [Item String] -> Context String
mkPostsCtx title subtitle posts =
    listField "posts" postCtx (return posts)
    <> constField "title" title
    <> constField "subtitle" subtitle
    <> defaultContext

main :: IO ()
main = do
    -- SEE: https://github.com/diku-dk/futhark-website/blob/4ebf2c19b8f9260124ab418ec82b951e28407241/site.hs#L30-L32
    syntax <-
        either (error . show) return
        =<< parseSyntaxDefinition "syntax/smt2.xml"

    hakyllWith config $ do

        let draftsPattern = fromGlob "drafts/*"
        let reviewsPattern = fromGlob "reviews/*"
        let postsPattern = fromGlob "posts/*"

        -- SEE: http://vapaus.org/text/hakyll-configuration.html
        mapM_ (directory static) ["css", "font", "js", "images", "pdf"]

        match "favicon/*.*" $ do
            route $ customRoute (flip replaceDirectory "" . toFilePath)
            compile copyFileCompiler

        -- SEE: https://groups.google.com/d/msg/hakyll/IhKmFO9vCIw/kC78nWp6CAAJ
        match "static/b.md" $ mkStatic "blockscope"
        match "static/p.md" $ mkStatic "philderbeast"
        match "static/cv.md" $ mkStatic "cv"
        match "static/project.md" $ mkStatic "project"
        match "static/contrib.md" $ mkStatic "project"
        match "static/tweet.md" $ mkStatic "tweet"

        -- SEE: http://javran.github.io/posts/2014-03-01-add-tags-to-your-hakyll-blog.html
        tags <- buildTags postsPattern (fromCapture "tags/*.html")

        match draftsPattern $ do
            route $ setExtension "html"
            compile $ do pandocPost postCtx (Just syntax)

        match reviewsPattern $ do
            route $ setExtension "html"
            compile $ do pandocPost postCtx (Just syntax)

        match postsPattern $ do
            route $ setExtension "html"
            compile $ do
                -- SEE: https://github.com/robwhitaker/hakyll-portfolio-blog/blob/729f2d51a1ff0d4f63e6a5cf4fc1b42cd6468d0b/site.hs#L146
                let ctx = tagsFieldNonEmpty "tags" tags <> postCtx
                pandocPost ctx (Just syntax)

        tagsRules tags $ \tag tagPattern -> do
            route idRoute
            compile $ do
                posts <- loadAll tagPattern >>= recentFirst

                let ctx =
                        constField "tag" tag
                        <> listField "posts" postCtx (return posts)
                        <> defaultContext

                makeItem ""
                    >>= loadAndApplyTemplate "templates/tag.html" ctx
                    >>= loadAndApplyTemplate "templates/default.html" ctx
                    >>= relativizeUrls

        create ["draft/index.html"] $ do
            route idRoute
            compile $ do
                posts <- loadAll reviewsPattern >>= recentFirst
                let ctx = mkPostsCtx "Sneak Peek" "Shine a light on this." posts
                pandocPost ctx (Just syntax)

        create ["review/index.html"] $ do
            route idRoute
            compile $ do
                posts <- loadAll reviewsPattern >>= recentFirst
                let ctx = mkPostsCtx "Up for Review" "Looking in the rear view mirror." posts
                pandocPost ctx (Just syntax)

        create ["post/index.html"] $ do
            route idRoute
            compile $ do
                posts <- loadAll postsPattern >>= recentFirst
                let ctx = mkPostsCtx "Post it, Notes" "Power-on, self-test." posts
                pandocPost ctx (Just syntax)

        match "static/index.md" $ do
            route . customRoute $ const "index.html"
            compile $ do pandocTemplate "templates/index.html" postCtx Nothing

        match "templates/*" $ compile templateCompiler

postCtx :: Context String
postCtx = dateField "date" "%Y-%m-%d" <> defaultContext

tagsFieldNonEmpty :: String -> Tags -> Context a
tagsFieldNonEmpty =
    tagsFieldWith getTagsNonEmpty simpleRenderLink (mconcat . intersperse " ")
    where
        getTagsNonEmpty :: Identifier -> Compiler [String]
        getTagsNonEmpty identifier = do
            tags <- getTags identifier
            if null tags then empty else return tags

        simpleRenderLink :: String -> Maybe FilePath -> Maybe H.Html
        simpleRenderLink _ Nothing = Nothing
        simpleRenderLink tag (Just filePath) =
            Just
            $ H.a
                ! A.class_ "badge badge-light"
                ! A.href (toValue $ toUrl filePath)
            $ toHtml tag
