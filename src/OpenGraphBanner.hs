{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module OpenGraphBanner
  ( localizedBannerCompiler
  , localizedBannerRoute
  , openGraphImageCtx
  ) where

import           System.Directory                ( doesFileExist )
import           System.FilePath

import qualified Data.ByteString.Lazy            as BL
import           Data.Char                       ( ord )
import           Data.List                       ( intercalate
                                                 , isInfixOf
                                                 , stripPrefix
                                                 )
import           Data.List.Extra                 ( splitOn )
import qualified Data.Text                       as T
import qualified Data.Text.Encoding              as T
import           Data.Yaml                       ( Value(..) )

import           Hakyll.Core.Compiler
import           Hakyll.Core.Compiler.Internal
import           Hakyll.Core.Dependencies
import           Hakyll.Core.Identifier
import           Hakyll.Core.Item
import           Hakyll.Core.UnixFilter
import           Hakyll.Web.ExtendedTemplate
import           Hakyll.Web.ExtendedTemplate.Type

import           Multilingual                    ( loadYamlObject )

openGraphImageCtx :: String -> String -> Context a
openGraphImageCtx lc siteRoot = field "og_image" $ \item -> do
  theme <- stringFromPageConfig item "theme"
  year <- stringFromPageConfig item "year"
  let banner = theme ++ "-" ++ year ++ "-banner"
      source = "assets/img/" ++ banner ++ ".svg"
      url = siteRoot ++ "/img/" ++ banner ++ "-" ++ lc ++ ".png"
  exists <- unsafeCompiler $ doesFileExist source
  if exists
    then return . String . T.pack $ url
    else noResult $ "No OpenGraph banner SVG at " ++ source
 where
  stringFromPageConfig item key = do
    value <- getContext (metadataField <> configField "config.yaml") key item
    case value of
      String text -> return $ T.unpack text
      other -> fail $ "OpenGraph image field expected string '" ++ key ++
        "', got " ++ fieldType other

localizedBannerRoute :: String -> Identifier -> FilePath
localizedBannerRoute lc id =
  "img" </> takeBaseName (toFilePath id) ++ "-" ++ lc <.> "png"

localizedBannerCompiler :: String -> Compiler (Item BL.ByteString)
localizedBannerCompiler lc = do
  item <- getResourceString
  (_, year) <- either fail return $ bannerStemParts (itemIdentifier item)
  title <- bannerTitle lc year
  overlaid <- addTitleOverlay title (itemBody item)
  makeItem (utf8LBS overlaid)
    >>= withItemBody (unixFilterLBS "rsvg-convert" ["--format=png"])

bannerStemParts :: Identifier -> Either String (String, String)
bannerStemParts id = do
  stem <- maybe (Left msg) Right . stripSuffix' "-banner" $ takeBaseName (toFilePath id)
  case reverse (splitOn "-" stem) of
    year : themeParts
      | not (null themeParts) -> Right (intercalate "-" (reverse themeParts), year)
    _ -> Left msg
 where
  msg = "Cannot derive theme and year from banner filename: " ++ show id

stripSuffix' :: Eq a => [a] -> [a] -> Maybe [a]
stripSuffix' suffix text = reverse <$> stripPrefix (reverse suffix) (reverse text)

bannerTitle :: String -> String -> Compiler String
bannerTitle lc year = do
  let fp = "content" </> year </> "config.yaml"
  compilerTellDependencies [IdentifierDependency (fromFilePath fp)]
  metadata <- unsafeCompiler $ loadYamlObject fp
  value <- metadataJSON metadata (fromFilePath fp) (lc ++ ".title")
  case value of
    String title -> return $ T.unpack title
    other -> fail $ "OpenGraph banner title expected string '" ++ lc ++
      ".title', got " ++ fieldType other

addTitleOverlay :: String -> String -> Compiler String
addTitleOverlay title svg
  | closingSvg `isInfixOf` svg =
      let overlay = titleOverlay title
          (before : after) = splitOn closingSvg svg
      in return $ before ++ overlay ++ closingSvg ++ intercalate closingSvg after
  | otherwise = fail "Cannot add OpenGraph title overlay: no closing </svg> tag found."
 where
  closingSvg = "</svg>"

titleOverlay :: String -> String
titleOverlay title =
  "\n  <defs>\n" ++
  "    <filter id=\"og-title-shadow\" x=\"-20%\" y=\"-40%\" width=\"140%\" height=\"180%\">\n" ++
  "      <feDropShadow dx=\"0\" dy=\"0\" stdDeviation=\"2\" flood-color=\"#000000\" flood-opacity=\"1\" />\n" ++
  "    </filter>\n" ++
  "  </defs>\n" ++
  "  <text x=\"50%\" y=\"72%\" text-anchor=\"middle\" dominant-baseline=\"middle\" " ++
  "font-family=\"Noto Sans CJK TC, Noto Sans CJK, sans-serif\" font-weight=\"700\" " ++
  "font-size=\"" ++ show fontSize ++ "\" fill=\"#ffffff\" filter=\"url(#og-title-shadow)\">\n" ++
  concat (zipWith tspan tspans lines') ++
  "  </text>\n"
 where
  lines' = wrappedTitleLines title
  fontSize = if length lines' == 1 then 24 :: Int else 17
  tspans = if length lines' == 1 then ["0"] else ["-0.55em", "1.2em"]
  tspan dy line =
    "    <tspan x=\"50%\" dy=\"" ++ dy ++ "\">" ++ escapeXml line ++ "</tspan>\n"

wrappedTitleLines :: String -> [String]
wrappedTitleLines title
  | visualLength title <= 28 = [title]
  | otherwise = case wrapWords 34 title of
      [] -> [title]
      [line] -> [line]
      line : rest -> [line, unwords rest]

wrapWords :: Int -> String -> [String]
wrapWords limit = foldr addWord [] . words
 where
  addWord word [] = [word]
  addWord word lines'@(line : rest)
    | visualLength word + 1 + visualLength line <= limit = (word ++ " " ++ line) : rest
    | otherwise = word : lines'

visualLength :: String -> Int
visualLength = sum . map (\c -> if ord c < 128 then 1 else 2)

escapeXml :: String -> String
escapeXml = concatMap $ \case
  '&' -> "&amp;"
  '<' -> "&lt;"
  '>' -> "&gt;"
  '"' -> "&quot;"
  '\'' -> "&apos;"
  c -> [c]

utf8LBS :: String -> BL.ByteString
utf8LBS = BL.fromStrict . T.encodeUtf8 . T.pack
