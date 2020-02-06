{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BlockArguments #-}
import           Data.Monoid (mappend)
import           Hakyll

main :: IO ()
main = hakyll $ do
    createRedirects [("index.html", "zh/index.html")]

    match "content/**.zh.html" $ do
        route $ gsubRoute "content/" (const "zh/")
          `composeRoutes` gsubRoute "zh.html" (const "html")
        compile $ do
            let indexCtx = langField "zh" <> biURL <> defaultContext
            getResourceBody
                >>= applyAsTemplate indexCtx
                >>= loadAndApplyTemplate "templates/default.zh.html" indexCtx
                >>= loadAndApplyTemplate "templates/head.html" indexCtx
                >>= relativizeUrls

    match "content/**.en.html" $ do
        route $ gsubRoute "content/" (const "en/")
          `composeRoutes` gsubRoute "en.html" (const "html")
        compile $ do
            let indexCtx = langField "en" <> biURL <> defaultContext
            getResourceBody
                >>= applyAsTemplate indexCtx
                >>= loadAndApplyTemplate "templates/default.en.html" indexCtx
                >>= loadAndApplyTemplate "templates/head.html" indexCtx
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

langField :: String -> Context a
langField = constField "lang" 

-- Produce the URL to its English/Chinese version of a given context
biURL :: Context a
biURL = field "en-url" (fmap (substUpDom "en") . getURL)
  <> field "zh-url" (fmap (substUpDom "zh") . getURL)

-- An ad hoc approach to change /xxx/yyy to /dom/yyy
substUpDom :: String -> String -> String
substUpDom dom = ('/':) . (dom ++) . dropWhile (/= '/') . tail

getURL :: Item a -> Compiler String
getURL i = do
    let id = itemIdentifier i
        empty' = fail $ "No route url found for item " ++ show id
    maybe empty' toUrl <$> getRoute id