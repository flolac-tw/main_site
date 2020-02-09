{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BlockArguments #-}
import           Data.Monoid (mappend)
import           Control.Monad
import           Hakyll

import           Multilingual

main :: IO ()
main = hakyll $ do
    createRedirects [("index.html", "zh/index.html")]

    forM_ ["zh", "en"] $ \lc -> match "content/**.html" $ version lc $ do
        route $ gsubRoute "content/" (const $ lc ++ "/")
        compile $ do
            let ctx = defaultContext <> localeCtx lc <> langToggleURL lc
            getResourceBody
                >>= applyAsTemplate ctx
                >>= loadAndApplyTemplate "templates/default.html" ctx
                >>= loadAndApplyTemplatesLC lc ctx
                  ["templates/nav.html", "templates/footer.html"]
                >>= loadAndApplyTemplate "templates/head.html" ctx
                >>= relativizeUrls

    match "assets/img/**" $ do
        route   (gsubRoute "assets/" (const ""))
        compile copyFileCompiler

    match "assets/css/**" $ do
        route   (gsubRoute "assets/" (const ""))
        compile compressCssCompiler

    match "templates/*" $ compile templateBodyCompiler

{-
    match "content/posts/*" $ do
        route $ gsubRoute "content/" (const "") `composeRoutes` setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/post.html"    postCtx
            >>= loadAndApplyTemplate "templates/default.html" postCtx
            >>= relativizeUrls

    create ["archive.html"] $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "content/posts/*"
            let archiveCtx =
                    listField "posts" postCtx (return posts) <>
                    defaultContext                           <>
                    constField "title" "Archives"

            makeItem ""
                >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
                >>= loadAndApplyTemplate "templates/default.html" archiveCtx
                >>= relativizeUrls
-}


--------------------------------------------------------------------------------
postCtx :: Context String
postCtx =
    dateField "date" "%B %e, %Y" <>
    defaultContext

------------------------------------------------------------------------------
-- Produce the URL to its English/Chinese version of a given context
langToggleURL :: String -> Context a
langToggleURL lc = field "LC-toggle-url" $ case lc of
    "zh" -> fmap (substUpDom "en") . getURL
    "en" -> fmap (substUpDom "zh") . getURL
    _    -> getURL
