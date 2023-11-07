{-# LANGUAGE OverloadedStrings #-}

import Control.Applicative (empty)
import qualified Data.Map as Map
import Data.List (intersperse, stripPrefix)
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
import System.Environment (getArgs)

data Syntaxes =
    Syntaxes
        { syntaxSMT2 :: Maybe Syntax
        , syntaxDhall :: Maybe Syntax
        }

nullSyntaxes :: Syntaxes
nullSyntaxes = Syntaxes Nothing Nothing

pandocReaderOptions :: Syntaxes -> ReaderOptions
pandocReaderOptions Syntaxes{syntaxSMT2 = Nothing, syntaxDhall = Nothing} = defaultHakyllReaderOptions
pandocReaderOptions Syntaxes{syntaxSMT2 = Just _, syntaxDhall = Just _} = defaultHakyllReaderOptions { readerIndentedCodeClasses = ["smt2", "dhall"] }
pandocReaderOptions Syntaxes{syntaxSMT2 = Just _, syntaxDhall = Nothing} = defaultHakyllReaderOptions { readerIndentedCodeClasses = ["smt2"] }
pandocReaderOptions Syntaxes{syntaxSMT2 = Nothing, syntaxDhall = Just _} = defaultHakyllReaderOptions { readerIndentedCodeClasses = ["dhall"] }

pandocWriterOptions :: Syntaxes -> WriterOptions
pandocWriterOptions Syntaxes{syntaxSMT2 = smt2, syntaxDhall = dhall} =
        defaultHakyllWriterOptions
            { writerHTMLMathMethod = MathJax ""
            , writerSyntaxMap =
                maybe id (Map.insert "smt2") smt2
                . maybe id (Map.insert "dhall") dhall
                $ writerSyntaxMap defaultHakyllWriterOptions
            }

pandoc :: Syntaxes -> Compiler (Item String)
pandoc syntaxes =
    pandocCompilerWith (pandocReaderOptions syntaxes) (pandocWriterOptions syntaxes)

static :: Pattern -> Rules ()
static f = match f $ do
    route idRoute
    compile copyFileCompiler

directory :: (Pattern -> Rules a) -> String -> Rules a
directory act f = act $ fromGlob $ f ++ "/**"

config :: Configuration
config = defaultConfiguration { providerDirectory = "." }

mkStatic :: FilePath -> Rules ()
mkStatic template = do
    -- SEE: https://aherrmann.github.io/programming/2016/01/31/jekyll-style-urls-with-hakyll/
    route
        $ gsubRoute "static/" (const "")
        `composeRoutes`
        customRoute ((</> "index.html") . fst . splitExtension . toFilePath)

    compile $ do
        pandoc nullSyntaxes
            >>= loadAndApplyTemplate "templates/about.html" postCtx
            >>= loadAndApplyTemplate (fromFilePath $ template <.> ".html") postCtx
            >>= relativizeUrls

mkArticle :: Identifier -> Syntaxes -> Context String -> Compiler (Item String)
mkArticle articleTemplate syntaxes ctx =
    pandoc syntaxes
        >>= loadAndApplyTemplate "templates/posts/post.html" ctx
        >>= loadAndApplyTemplate articleTemplate ctx
        >>= relativizeUrls

mkPost :: Syntaxes -> Context String -> Compiler (Item String)
mkPost = mkArticle "templates/posts/default.html"

mkReview :: Syntaxes -> Context String -> Compiler (Item String)
mkReview = mkArticle "templates/reviews/default.html"

mkDraft :: Syntaxes -> Context String -> Compiler (Item String)
mkDraft = mkArticle "templates/drafts/default.html"

mkPostsCtx :: String -> String -> [Item String] -> Context String
mkPostsCtx title subtitle posts =
    listField "posts" postCtx (return posts)
    <> constField "title" title
    <> constField "subtitle" subtitle
    <> defaultContext

mkArticleItem :: Identifier -> Context String -> Compiler (Item String)
mkArticleItem articleTemplate ctx =
    makeItem ""
        >>= loadAndApplyTemplate "templates/posts/archive.html" ctx
        >>= loadAndApplyTemplate articleTemplate ctx
        >>= relativizeUrls

mkPostItem :: Context String -> Compiler (Item String)
mkPostItem = mkArticleItem "templates/posts/default.html"

mkReviewItem :: Context String -> Compiler (Item String)
mkReviewItem = mkArticleItem "templates/reviews/default.html"

mkDraftItem :: Context String -> Compiler (Item String)
mkDraftItem = mkArticleItem "templates/drafts/default.html"

mkArticles :: Pattern -> (Context String -> Compiler (Item String)) -> String -> String -> Rules ()
mkArticles articlePattern mkArticle' title subtitle = compile $
    loadAll articlePattern >>= recentFirst >>= mkArticle' . mkPostsCtx title subtitle

main :: IO ()
main = do
    args <- getArgs

    -- SEE: https://github.com/diku-dk/futhark-website/blob/4ebf2c19b8f9260124ab418ec82b951e28407241/site.hs#L30-L32
    syntaxSMT2 <- either (error . show) return =<< parseSyntaxDefinition "syntax/smt2.xml"
    syntaxDhall <- either (error . show) return =<< parseSyntaxDefinition "syntax/dhall.xml"
    let syntaxes = Syntaxes (Just syntaxSMT2) (Just syntaxDhall)

    hakyllWith config $ do

        let postsPattern = fromGlob "posts/*"
        let reviewsPattern = fromGlob "reviews/*"

        -- NOTE: We don't want to publish drafts so they're watch only.
        let draftsPattern = if "watch" `elem` args then fromGlob "drafts/*" else ""

        -- SEE: http://vapaus.org/text/hakyll-configuration.html
        mapM_ (directory static) ["css", "font", "js", "images", "pdf"]

        match "favicon/*.*" $ do
            route $ customRoute (flip replaceDirectory "" . toFilePath)
            compile copyFileCompiler

        match "node_modules/typeface-et-book/et-book/et-book/**/*.*" $ do
            route $ customRoute (etBookFontRoute . toFilePath)
            compile copyFileCompiler

        match "node_modules/@fortawesome/fontawesome-free/webfonts/*.*" $ do
            route $ customRoute (faFontRoute . toFilePath)
            compile copyFileCompiler

        -- SEE: https://groups.google.com/d/msg/hakyll/IhKmFO9vCIw/kC78nWp6CAAJ
        match "static/b.md" $ mkStatic "templates/blockscope"
        match "static/p.md" $ mkStatic "templates/philderbeast"
        match "static/cv.md" $ mkStatic "templates/cv"

        match "static/projects/uom.md" $ do
            route . customRoute $ const "projects/uom/index.html"

            compile
                $ pandoc nullSyntaxes
                >>= loadAndApplyTemplate "templates/about.html" postCtx
                >>= loadAndApplyTemplate (fromFilePath $ "templates/projects/project.html") postCtx
                >>= relativizeUrls

        match "static/projects/geodetics.md" $ do
            route . customRoute $ const "projects/geodetics/index.html"

            compile
                $ pandoc nullSyntaxes
                >>= loadAndApplyTemplate "templates/about.html" postCtx
                >>= loadAndApplyTemplate (fromFilePath $ "templates/projects/project.html") postCtx
                >>= relativizeUrls

        match "static/projects/tooling.md" $ do
            route . customRoute $ const "projects/tooling/index.html"

            compile
                $ pandoc nullSyntaxes
                >>= loadAndApplyTemplate "templates/about.html" postCtx
                >>= loadAndApplyTemplate (fromFilePath "templates/projects/project.html") postCtx
                >>= relativizeUrls

        match "static/projects/fly.md" $ do
            route . customRoute $ const "projects/fly/index.html"

            compile
                $ pandoc nullSyntaxes
                >>= loadAndApplyTemplate "templates/about.html" postCtx
                >>= loadAndApplyTemplate (fromFilePath "templates/projects/project.html") postCtx
                >>= relativizeUrls

        match "static/projects/contrib.md" $ do
            route . customRoute $ const "projects/contrib/index.html"

            compile
                $ pandoc nullSyntaxes
                >>= loadAndApplyTemplate "templates/about.html" postCtx
                >>= loadAndApplyTemplate (fromFilePath "templates/projects/project.html") postCtx
                >>= relativizeUrls

        match "static/projects/index.md" $ do
            route . customRoute $ const "projects/index.html"

            compile
                $ pandoc nullSyntaxes
                >>= loadAndApplyTemplate "templates/about.html" postCtx
                >>= loadAndApplyTemplate (fromFilePath "templates/projects/project.html") postCtx
                >>= relativizeUrls

        -- SEE: http://javran.github.io/posts/2014-03-01-add-tags-to-your-hakyll-blog.html
        tags <- buildTags postsPattern (fromCapture "tags/*.html")

        match draftsPattern $ do
            route $ setExtension "html"
            compile $ do mkDraft syntaxes postCtx

        match reviewsPattern $ do
            route $ setExtension "html"
            compile $ do mkReview syntaxes postCtx

        match postsPattern $ do
            route $ setExtension "html"
            compile $ do
                -- SEE: https://github.com/robwhitaker/hakyll-portfolio-blog/blob/729f2d51a1ff0d4f63e6a5cf4fc1b42cd6468d0b/site.hs#L146
                let ctx = tagsFieldNonEmpty "tags" tags <> postCtx
                mkPost syntaxes ctx

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
            mkArticles draftsPattern mkDraftItem  "Sneak Peek" "Shine a light on these draft posts (not for publication)."

        create ["review/index.html"] $ do
            route idRoute
            mkArticles reviewsPattern mkReviewItem "Up for Review" "Looking in the rear view mirror."

        create ["blog/index.html"] $ do
            route idRoute
            mkArticles postsPattern mkPostItem "Post it, Notes" "Power-on, self-test."

        match "static/index.md" $ do
            route . customRoute $ const "index.html"
            compile
                $ pandoc nullSyntaxes
                >>= loadAndApplyTemplate "templates/index.html" postCtx
                >>= loadAndApplyTemplate "templates/default.html" postCtx
                >>= relativizeUrls

        match "templates/*" $ compile templateCompiler
        match "templates/posts/*" $ compile templateCompiler
        match "templates/reviews/*" $ compile templateCompiler
        match "templates/drafts/*" $ compile templateCompiler
        match "templates/projects/*" $ compile templateCompiler

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
                ! A.class_ "badge bg-light text-dark"
                ! A.href (toValue $ toUrl filePath)
            $ toHtml tag

etBookFontRoute :: FilePath -> FilePath
etBookFontRoute x
    | Just y <- stripPrefix "node_modules/typeface-et-book/et-book/" x
    = "css" </> y
    | otherwise = error $ "Unexpected et-book font of " ++ x

faFontRoute :: FilePath -> FilePath
faFontRoute x
    | Just y <- stripPrefix "node_modules/@fortawesome/fontawesome-free/webfonts/" x
    = "css" </> "fonts" </> y
    | otherwise = error $ "Unexpected fontawesome font of " ++ x