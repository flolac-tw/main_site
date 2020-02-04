{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BlockArguments #-} 
import           Data.Monoid (mappend)
import           Hakyll

main :: IO ()
main = hakyll $ do
    match "assets/img/**" $ do
        route   (gsubRoute "assets/" (const ""))
        compile copyFileCompiler

    match "assets/css/**" $ do
        route   (gsubRoute "assets/" (const ""))
        compile compressCssCompiler

    match (fromList ["content/about.rst", "content/contact.markdown"]) $ do
        route   $ gsubRoute "content/" (const "") `composeRoutes` setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/default.html" defaultContext
            >>= relativizeUrls

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
                    metadataField                            <>
                    listField "posts" postCtx (return posts) <>
                    constField "title" "Archives"            <>
                    defaultContext

            makeItem ""
                >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
                >>= loadAndApplyTemplate "templates/default.html" archiveCtx
                >>= relativizeUrls

    match "content/index.html" $ do
        route (gsubRoute "content/" (const ""))
        compile $ do
            let indexCtx =
                    metadataField  <>
                    defaultContext

            getResourceBody
                >>= applyAsTemplate indexCtx
                >>= loadAndApplyTemplate "templates/default.html" indexCtx
                >>= relativizeUrls

    match "templates/*" $ compile templateBodyCompiler


--------------------------------------------------------------------------------
postCtx :: Context String
postCtx =
    dateField "date" "%B %e, %Y" `mappend`
    defaultContext
