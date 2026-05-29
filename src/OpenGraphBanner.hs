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
import           Data.Char                       ( isSpace
                                                 , ord
                                                 )
import           Data.List                       ( intercalate
                                                 , stripPrefix
                                                 )
import           Data.List.Extra                 ( splitOn )
import qualified Data.Text                       as T
import qualified Data.Text.Encoding              as T
import           Data.Yaml                       ( Object
                                                 , Value(..)
                                                 )

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
  (title, subtitle) <- bannerText lc year
  svg <- openGraphSvg title subtitle (itemBody item)
  makeItem (utf8LBS svg)
    >>= withItemBody (unixFilterLBS "rsvg-convert"
          ["--format=png", "--width=1200", "--height=630"])

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

bannerText :: String -> String -> Compiler (String, String)
bannerText lc year = do
  let fp = "content" </> year </> "config.yaml"
  compilerTellDependencies [IdentifierDependency (fromFilePath fp)]
  metadata <- unsafeCompiler $ loadYamlObject fp
  title <- metadataString metadata fp (lc ++ ".title")
  subtitle <- metadataString metadata fp (lc ++ ".subtitle")
  return (title, subtitle)

metadataString :: Object -> FilePath -> String -> Compiler String
metadataString metadata fp key = do
  value <- metadataJSON metadata (fromFilePath fp) key
  case value of
    String text -> return $ T.unpack text
    other -> fail $ "OpenGraph banner text expected string '" ++ key ++
      "', got " ++ fieldType other

openGraphSvg :: String -> String -> String -> Compiler String
openGraphSvg title subtitle source = do
  artwork <- centeredArtwork source
  return $
    "<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"1200\" height=\"630\" viewBox=\"0 0 1200 630\">\n" ++
    artwork ++ "\n" ++
    titleOverlay title subtitle ++
    "</svg>\n"

centeredArtwork :: String -> Compiler String
centeredArtwork source =
  case stripPrefix "<svg" trimmed of
    Just attrs ->
      return $
        "  <svg x=\"0\" y=\"0\" width=\"1200\" height=\"630\" " ++
        "preserveAspectRatio=\"xMidYMid slice\"" ++ attrs
    Nothing -> fail "Cannot wrap OpenGraph banner artwork: no opening <svg> tag found."
 where
  trimmed = dropWhile isSpace $ stripXmlDeclaration source

stripXmlDeclaration :: String -> String
stripXmlDeclaration =
  T.unpack . stripDecl . T.stripStart . T.pack
 where
  stripDecl text
    | "<?xml" `T.isPrefixOf` text =
        let (_, after) = T.breakOn "?>" text
        in if T.null after
             then text
             else T.stripStart $ T.drop 2 after
    | otherwise = text

titleOverlay :: String -> String -> String
titleOverlay title subtitle =
  "\n  <defs>\n" ++
  "    <filter id=\"og-title-shadow\" x=\"-20%\" y=\"-40%\" width=\"140%\" height=\"180%\">\n" ++
  "      <feDropShadow dx=\"0\" dy=\"0\" stdDeviation=\"5\" flood-color=\"#000000\" flood-opacity=\"1\" />\n" ++
  "    </filter>\n" ++
  "  </defs>\n" ++
  textBlock titleY titleFont titleLines titleSpans ++
  textBlock subtitleY subtitleFont subtitleLines subtitleSpans
 where
  titleLines = wrappedTitleLines title
  subtitleLines = wrappedSubtitleLines subtitle
  titleFont = if length titleLines == 1 then 76 :: Int else 54
  subtitleFont = if length subtitleLines == 1 then 38 :: Int else 34
  titleY = if length titleLines == 1 then 405 :: Int else 380
  subtitleY = if length titleLines == 1 then 500 :: Int else 535
  titleSpans = if length titleLines == 1 then ["0"] else ["-0.55em", "1.2em"]
  subtitleSpans = if length subtitleLines == 1 then ["0"] else ["-0.45em", "1.15em"]
  textBlock y fontSize lines' tspans =
    "  <text x=\"600\" y=\"" ++ show y ++ "\" text-anchor=\"middle\" dominant-baseline=\"middle\" " ++
    "font-family=\"Noto Sans CJK TC, Noto Sans CJK, sans-serif\" font-weight=\"700\" " ++
    "font-size=\"" ++ show fontSize ++ "\" fill=\"#ffffff\" filter=\"url(#og-title-shadow)\">\n" ++
    concat (zipWith tspan tspans lines') ++
    "  </text>\n"
  tspan dy line =
    "    <tspan x=\"600\" dy=\"" ++ dy ++ "\">" ++ escapeXml line ++ "</tspan>\n"

wrappedTitleLines :: String -> [String]
wrappedTitleLines title
  | visualLength title <= 28 = [title]
  | otherwise = case wrapWords 34 title of
      [] -> [title]
      [line] -> [line]
      line : rest -> [line, unwords rest]

wrappedSubtitleLines :: String -> [String]
wrappedSubtitleLines subtitle
  | visualLength subtitle <= 42 = [subtitle]
  | otherwise = case wrapWords 42 subtitle of
      [] -> [subtitle]
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
