{-# LANGUAGE OverloadedStrings #-}
import System.FilePath

import           Control.Monad
import qualified Data.Text                     as T
import           Data.Monoid                    ( mappend )
import           Data.List                      ( intercalate )
import           Data.List.Extra                ( splitOn )
import           Data.Yaml                      ( Value(..) )

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
import           OpenGraphBanner

main :: IO ()
main = hakyll $ do
  let currentYear = "2026"
      siteRoot = "https://flolac.iis.sinica.edu.tw"
  createRedirects [("index.html", "zh/"++currentYear)]

  match "content/**/config.yaml" $ do
    route idRoute
    compile getResourceBody

  -- Liang-Ting (2023-05-02):
  -- TODO: Streamline the following two cases.
  forM_ ["zh", "en"] $ \lc -> match "content/*/registration.html" $ version lc $ do
    route $ gsubRoute "content/" (const $ lc ++ "/")
    compile $ do
      let baseCtx = constField "current_year" currentYear
                 <> constField "site_root" siteRoot
                 <> openGraphImageCtx lc siteRoot
                 <> defaultContext
                 <> configField "config.yaml"
                 <> i18nCtx lc
                 <> langToggleURL lc
          pageCtx = localize lc baseCtx

      -- Treat metadata as template as well
      appliedPage <- getResourceString >>= applyAsTemplate pageCtx
      let (metadata, _) = either mempty id $ parsePage $ itemBody appliedPage
          appliedMetadataField = Context $ \k _ -> do
                let empty' = noResult $ "No '"  ++ k ++ "' field in applied metadata."
                maybe empty' (return . String . T.pack) (lookupString k metadata)
          ctx' = appliedMetadataField <> baseCtx
          pageCtx' = localize lc ctx'
      getResourceBody
        >>= applyAsTemplate pageCtx'
        >>= loadAndApplyTemplatesLC
              lc
              ctx'
              [ "templates/banner.html"
              , "templates/nav.html"
              , "templates/footer.html"
              , "templates/head.html"
              ]
        >>= relativizeUrls

  forM_ ["zh", "en"] $ \lc -> match "content/*/index.html" $ version lc $ do
    route $ gsubRoute "content/" (const $ lc ++ "/")
    compile $ do
      let baseCtx = constField "current_year" currentYear
                 <> constField "site_root" siteRoot
                 <> openGraphImageCtx lc siteRoot
                 <> constField "header_show_year" "true"
                 <> defaultContext
                 <> configField "config.yaml"
                 <> i18nCtx lc
                 <> langToggleURL lc
          pageCtx = localize lc baseCtx

      -- Treat metadata as template as well
      appliedPage <- getResourceString >>= applyAsTemplate pageCtx
      let (metadata, _) = either mempty id $ parsePage $ itemBody appliedPage
          appliedMetadataField = Context $ \k _ -> do
                let empty' = noResult $ "No '"  ++ k ++ "' field in applied metadata."
                maybe empty' (return . String . T.pack) (lookupString k metadata)
          ctx' = appliedMetadataField <> baseCtx
          pageCtx' = localize lc ctx'
      getResourceBody
        >>= applyAsTemplate pageCtx'
        >>= loadAndApplyTemplatesLC
              lc
              ctx'
              [ "templates/course-index.html"
              , "templates/banner.html"
              , "templates/nav.html"
              , "templates/footer.html"
              , "templates/head.html"
              ]
        >>= relativizeUrls

  forM_ ["zh", "en"] $ \lc -> match "assets/img/*-banner.svg" $ version ("og-" ++ lc) $ do
    route $ customRoute (localizedBannerRoute lc)
    compile $ localizedBannerCompiler lc

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

data Theme = Ocean | Mountain

themeCtx :: Theme -> Context a
themeCtx th = case th of
                Ocean -> stringField "theme" (const $ return "ocean")
                Mountain -> stringField "theme" (const $ return "mountain")
