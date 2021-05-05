{-# LANGUAGE OverloadedStrings #-}
import           Control.Monad
import qualified Data.Text                     as T
import           Data.Monoid                    ( mappend )
import           Data.List                      ( intercalate )
import           Data.List.Extra                ( splitOn )
import           Data.Yaml

import           Hakyll.Core
import           Hakyll.Main
import           Hakyll.Web.CompressCss
import           Hakyll.Web.Html
import           Hakyll.Web.Html.RelativizeUrls

import           Hakyll.Web.ExtendedTemplate
import           Hakyll.Web.ExtendedTemplate.Type

import           Redirect
import           Multilingual

main :: IO ()
main = hakyll $ do
  createRedirects [("index.html", "zh/2021/index.html")]

  forM_ ["zh", "en"] $ \lc -> match "content/**.html" $ version lc $ do
    route $ gsubRoute "content/" (const $ lc ++ "/")
    compile $ do
      let ctx = defaultContext <> localeCtx lc <> langToggleURL lc
      getResourceBody
        >>= applyAsTemplate ctx
        >>= loadAndApplyTemplatesLC
              lc
              ctx
              [ "templates/banner.html"
              , "templates/nav.html"
              , "templates/footer.html"
              , "templates/head.html"
              ]
        >>= relativizeUrls

  match "assets/img/**" $ do
    route (gsubRoute "assets/" (const ""))
    compile copyFileCompiler

  match "assets/script/**" $ do
    route (gsubRoute "assets/" (const ""))
    compile copyFileCompiler

  match "assets/css/**" $ do
    route (gsubRoute "assets/" (const ""))
    compile compressCssCompiler

  match "assets/html/**" $ do
    route (gsubRoute "assets/html/" (const ""))
    compile copyFileCompiler

  match "templates/*" $ compile templateBodyCompiler

------------------------------------------------------------------------------
-- Produce a URL to its English/Chinese version of a given context
langToggleURL :: String -> Context a
langToggleURL lc = field "LC-toggle-url" $ case lc of
  "zh" -> fmap (String . T.pack . substRoot "en") . getURL
  "en" -> fmap (String . T.pack . substRoot "zh") . getURL
  _    -> fmap (String . T.pack) . getURL

getURL :: Item a -> Compiler String
getURL i = maybe empty' toUrl <$> getRoute id
 where
  id     = itemIdentifier i
  empty' = fail $ "No route url found for item " ++ show id

-- An ad-hoc function of changing from /xxx/yyy to /dom/yyy
substRoot :: String -> String -> String
substRoot dom = intercalate "/" . ([[], dom] ++) . drop 2 . splitOn "/"
