{-# LANGUAGE OverloadedStrings #-}
import System.FilePath

import           Control.Monad
import qualified Data.Text                     as T
import           Data.Monoid                    ( mappend )
import           Data.List                      ( intercalate )
import           Data.List.Extra                ( splitOn )
import           Data.Yaml

import           Hakyll.Core.Compiler
import           Hakyll.Core.Configuration
import           Hakyll.Core.File
import           Hakyll.Core.Identifier
import           Hakyll.Core.Identifier.Pattern
import           Hakyll.Core.Item
import           Hakyll.Core.Metadata
import           Hakyll.Core.Provider.Metadata
import           Hakyll.Core.Routes
import           Hakyll.Core.Rules
import           Hakyll.Core.UnixFilter
import           Hakyll.Core.Util.File
import           Hakyll.Core.Util.String
import           Hakyll.Core.Writable


import           Hakyll.Main
import           Hakyll.Web.CompressCss
import           Hakyll.Web.Html
import           Hakyll.Web.Html.RelativizeUrls

import           Hakyll.Web.ExtendedTemplate
import           Hakyll.Web.ExtendedTemplate.Type

import           Hakyll.Web.Sass ( sassCompiler )
import           Redirect
import           Multilingual
import           YearlyTheme

main :: IO ()
main = hakyll $ do
  let currentYear = "2022"
  createRedirects [("index.html", "zh/"++currentYear++"/index.html")]

  forM_ ["zh", "en"] $ \lc -> match "content/**.html" $ version lc $ do
    route $ gsubRoute "content/" (const $ lc ++ "/")
    compile $ do
      let ctx = constField "current_year" currentYear
             <> importField
             <> defaultContext
             <> localeCtx lc
             <> langToggleURL lc

      -- Treat metadata as template as well
      appliedPage <- getResourceString >>= applyAsTemplate ctx
      let (metadata, _) = either mempty id $ parsePage $ itemBody appliedPage
          appliedMetadataField = Context $ \k _ -> do
                let empty' = noResult $ "No '"  ++ k ++ "' field in applied metadata."
                maybe empty' (return . String . T.pack) (lookupString k metadata)
          ctx' = appliedMetadataField <> ctx
      getResourceBody
        >>= applyAsTemplate ctx'
        >>= loadAndApplyTemplatesLC
              lc
              ctx'
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

  scssDependency <- makePatternDependency "assets/scss/style.scss"
  rulesExtraDependencies [scssDependency]
    $ match "assets/scss/custom.scss"
    $ do
      route $ setExtension "css" `composeRoutes` gsubRoute "assets/scss/" (const "css/")
      compile (fmap compressCss <$> sassCompiler)

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
