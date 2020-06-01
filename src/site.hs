{-# LANGUAGE OverloadedStrings #-}
import           Data.Monoid (mappend)
import           Control.Monad
import           Hakyll

import           Multilingual
import           Util.List

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

    match "assets/html/**" $ do
        route   (gsubRoute "assets/html/" (const ""))
        compile copyFileCompiler

    match "templates/*" $ compile templateBodyCompiler

------------------------------------------------------------------------------
-- Produce a URL to its English/Chinese version of a given context
langToggleURL :: String -> Context a
langToggleURL lc = field "LC-toggle-url" $ case lc of
  "zh" -> fmap (substRoot "en") . getURL
  "en" -> fmap (substRoot "zh") . getURL
  _    -> getURL

getURL :: Item a -> Compiler String
getURL i = maybe empty' toUrl <$> getRoute id
  where
    id = itemIdentifier i
    empty' = fail $ "No route url found for item " ++ show id

-- An ad-hoc function of changing from /xxx/yyy to /dom/yyy
substRoot :: String -> String -> String
substRoot dom = joinOn '/' . ([[], dom ] ++) . drop 2 . splitOn '/'