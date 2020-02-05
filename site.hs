-- TODO: 
--   * Detect user's language and redirect index.html to zh/index.html or en/index.html 
--   * Treat everything ending with .zh.<ext> as in a root directory http://url/zh/
--    Similarly, .en.<ext> as in a root http://url/en/ 

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

    match "content/index.html" $ do
        route $ gsubRoute "content/" (const "")
        compile copyFileCompiler

    match "content/**.zh.html" $ do
        route $ gsubRoute "content/" (const "zh/")
          `composeRoutes` gsubRoute "zh.html" (const "html")
        compile $ do
            let indexCtx = enURL <> defaultContext
            getResourceBody
                >>= applyAsTemplate indexCtx
                >>= loadAndApplyTemplate "templates/default.zh.html" indexCtx
                >>= relativizeUrls

    match "content/**.en.html" $ do
        route $ gsubRoute "content/" (const "en/")
          `composeRoutes` gsubRoute "en.html" (const "html")
        compile $ do
            let indexCtx = zhURL <> defaultContext
            getResourceBody
                >>= applyAsTemplate indexCtx
                >>= loadAndApplyTemplate "templates/default.en.html" indexCtx
                >>= relativizeUrls

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

-- Produce the URL to its English/Chinese version of a given context
enURL :: Context a
enURL = field "en-url" (fmap (substUpDom "en") . getURL)

zhURL :: Context a
zhURL = field "zh-url" (fmap (substUpDom "zh") . getURL)

-- An ad hoc approach to change /xxx/yyy to /dom/yyy
substUpDom :: String -> String -> String
substUpDom dom = ('/':) . (dom ++) . dropWhile (/= '/') . tail

getURL :: Item a -> Compiler String
getURL i = do
    let id = itemIdentifier i
        empty' = fail $ "No route url found for item " ++ show id
    maybe empty' toUrl <$> getRoute id