{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
{-# LANGUAGE ExistentialQuantification     #-}
{-# LANGUAGE LambdaCase                    #-}
{-# LANGUAGE OverloadedStrings             #-}

module Hakyll.Web.ExtendedTemplate.Context
    ( ContextField (..)
    , Context (..)
    , fieldType
    , field
    , missingField
    , stringField

    , defaultContext
    , bodyField
    , urlField
    , pathField
    , titleField
    , dateField
    , dateFieldWith

    , getItemUTC
    , getItemModificationTime
    , modificationTimeField
    , modificationTimeFieldWith
    , teaserField
    , teaserFieldWithSeparator
    , metadataField
    , metadataIdField
    , metadataJSON
    , fromValue 
    ) where

--------------------------------------------------------------------------------
import           Control.Monad                 (msum)
import           Control.Monad.Fail            (MonadFail)
import qualified Data.HashMap.Strict           as HM
import           Data.List                     (intercalate, tails)
import           Data.Scientific               as S
import           Data.Text                     (Text)
import qualified Data.Text                     as T
import           Data.Time.Clock               (UTCTime (..))
import           Data.Time.Format              (formatTime, parseTimeM)
import           Data.Time.Locale.Compat       (TimeLocale, defaultTimeLocale)
import qualified Data.Vector                   as V
import           Data.Yaml                     (Object, Value(..))
import qualified Data.Yaml                     as Y

import           System.FilePath               (dropExtension, splitDirectories,
                                                takeBaseName)
import           Hakyll.Core.Compiler
import           Hakyll.Core.Compiler.Internal
import           Hakyll.Core.Identifier
import           Hakyll.Core.Item
import           Hakyll.Core.Metadata
import           Hakyll.Core.Provider
import           Hakyll.Core.Util.String       (needlePrefix, splitAll)
import           Hakyll.Web.Html

import           Hakyll.Web.ExtendedTemplate.Type

import           Prelude hiding (lookup)

--------------------------------------------------------------------------------
-- | Constructs a new field for a 'Context'.
-- If the key matches, the compiler is run and its result is substituted in the
-- template.
--
-- If the compiler fails, the field will be considered non-existent
-- in an @$if()$@ macro or ultimately break the template application.
-- Use 'empty' or 'noResult' for intentional failures of fields used in
-- @$if()$@, to distinguish them from exceptions thrown with 'fail'.
field :: String -> (Item a -> Compiler ContextField) -> Context a
field key f = Context $ \k item -> if key == k 
    then f item
    else noResult $ "Tried field: " ++ k

--------------------------------------------------------------------------------
-- | Constantly reports any field as missing. Mostly for internal usage,
-- it is the last choice in every context used in a template application.
missingField :: Context a
missingField = Context $ \k _ -> noResult $
    "Missing field '" ++ k ++ "' in context"

stringField
    :: String
    -> (Item a -> Compiler String)
    -> Context a
stringField key f = 
    field key $ fmap (String . T.pack) . f

--------------------------------------------------------------------------------
-- | A context that contains (in that order)
--
--     1. A @$body$@ field
--     2. Metadata fields
--     3. A @$url$@ 'urlField'
--     4. A @$path$@ 'pathField'
--     5. A @$title$@ 'titleField'

defaultContext :: Context String
defaultContext =
    bodyField     "body"     <>
    metadataField            <> 
    urlField      "url"      <> 
    pathField     "path"     <> 
    titleField    "title"

--------------------------------------------------------------------------------
-- | Constructs a 'field' that contains the body of the item.
bodyField :: String -> Context String
bodyField key = stringField key (return . itemBody)

--------------------------------------------------------------------------------
-- | Absolute url to the resulting item
urlField :: String -> Context a
urlField key = stringField key $ \i -> do
    let id = itemIdentifier i
        empty' = fail $ "No route url found for item " ++ show id
    maybe empty' toUrl <$> getRoute id

--------------------------------------------------------------------------------
-- | Filepath of the underlying file of the item
pathField :: String -> Context a
pathField key = stringField key $ return . toFilePath . itemIdentifier

--------------------------------------------------------------------------------
-- | This title 'field' takes the basename of the underlying file by default
titleField :: String -> Context a
titleField key = stringField key $ return . takeBaseName . toFilePath . itemIdentifier

--------------------------------------------------------------------------------
-- | When the metadata has a field called @published@ in one of the
-- following formats then this function can render the date.
--
--   * @Mon, 06 Sep 2010 00:01:00 +0000@
--
--   * @Mon, 06 Sep 2010 00:01:00 UTC@
--
--   * @Mon, 06 Sep 2010 00:01:00@
--
--   * @2010-09-06T00:01:00+0000@
--
--   * @2010-09-06T00:01:00Z@
--
--   * @2010-09-06T00:01:00@
--
--   * @2010-09-06 00:01:00+0000@
--
--   * @2010-09-06 00:01:00@
--
--   * @September 06, 2010 00:01 AM@
--
-- Following date-only formats are supported too (@00:00:00@ for time is
-- assumed)
--
--   * @2010-09-06@
--
--   * @September 06, 2010@
--
-- Alternatively, when the metadata has a field called @path@ in a
-- @folder/yyyy-mm-dd-title.extension@ format (the convention for pages)
-- and no @published@ metadata field set, this function can render
-- the date. This pattern matches the file name or directory names
-- that begins with @yyyy-mm-dd@ . For example:
-- @folder//yyyy-mm-dd-title//dist//main.extension@ .
-- In case of multiple matches, the rightmost one is used.
--
-- As another alternative, if none of the above matches, and the file has a
-- path which contains nested directories specifying a date, then that date
-- will be used. In other words, if the path is of the form
-- @**//yyyy//mm//dd//**//main.extension@ .
-- As above, in case of multiple matches, the rightmost one is used.

dateField :: String     -- ^ Key in which the rendered date should be placed
          -> String     -- ^ Format to use on the date
          -> Context a  -- ^ Resulting context
dateField = dateFieldWith defaultTimeLocale

--------------------------------------------------------------------------------
-- | This is an extended version of 'dateField' that allows you to
-- specify a time locale that is used for outputting the date. For more
-- details, see 'dateField' and 'formatTime'.
dateFieldWith :: TimeLocale  -- ^ Output time locale
              -> String      -- ^ Destination key
              -> String      -- ^ Format to use on the date
              -> Context a   -- ^ Resulting context
dateFieldWith locale key format = stringField key $ \i -> do
    time <- getItemUTC locale $ itemIdentifier i
    return $ formatTime locale format time
--------------------------------------------------------------------------------
-- | Parser to try to extract and parse the time from the @published@
-- field or from the filename. See 'dateField' for more information.
-- Exported for user convenience.
getItemUTC :: (MonadMetadata m, MonadFail m)
           => TimeLocale        -- ^ Output time locale
           -> Identifier        -- ^ Input page
           -> m UTCTime         -- ^ Parsed UTCTime
getItemUTC locale id' = do
    metadata <- getMetadata id'
    let tryField k fmt = lookupString k metadata >>= parseTime' fmt
        paths          = splitDirectories $ (dropExtension . toFilePath) id'

    maybe empty' return $ msum $
        [tryField "published" fmt | fmt <- formats] ++
        [tryField "date"      fmt | fmt <- formats] ++
        [parseTime' "%Y-%m-%d" $ intercalate "-" $ take 3 $ splitAll "-" fnCand | fnCand <- reverse paths] ++
        [parseTime' "%Y-%m-%d" $ intercalate "-" $ fnCand | fnCand <- map (take 3) $ reverse . tails $ paths]
  where
    empty'     = fail $ "Hakyll.Web.Template.Context.getItemUTC: " ++
        "could not parse time for " ++ show id'
    parseTime' = parseTimeM True locale
    formats    =
        [ "%a, %d %b %Y %H:%M:%S %Z"
        , "%a, %d %b %Y %H:%M:%S"
        , "%Y-%m-%dT%H:%M:%S%Z"
        , "%Y-%m-%dT%H:%M:%S"
        , "%Y-%m-%d %H:%M:%S%Z"
        , "%Y-%m-%d %H:%M:%S"
        , "%Y-%m-%d"
        , "%B %e, %Y %l:%M %p"
        , "%B %e, %Y"
        , "%b %d, %Y"
        ]


--------------------------------------------------------------------------------
-- | Get the time on which the actual file was last modified. This only works if
-- there actually is an underlying file, of couse.
getItemModificationTime
    :: Identifier
    -> Compiler UTCTime
getItemModificationTime identifier = do
    provider <- compilerProvider <$> compilerAsk
    return $ resourceModificationTime provider identifier

--------------------------------------------------------------------------------
-- | Creates a field with the last modification date of the underlying item.
modificationTimeField :: String     -- ^ Key
                      -> String     -- ^ Format
                      -> Context a  -- ^ Resulting context
modificationTimeField = modificationTimeFieldWith defaultTimeLocale

--------------------------------------------------------------------------------
-- | Creates a field with the last modification date of the underlying item
-- in a custom localisation format (see 'formatTime').
modificationTimeFieldWith :: TimeLocale  -- ^ Time output locale
                          -> String      -- ^ Key
                          -> String      -- ^ Format
                          -> Context a   -- ^ Resulting context
modificationTimeFieldWith locale key fmt = stringField key $ \i -> do
    mtime <- getItemModificationTime $ itemIdentifier i
    return $ formatTime locale fmt mtime

--------------------------------------------------------------------------------
-- | A context with "teaser" key which contain a teaser of the item.
-- The item is loaded from the given snapshot (which should be saved
-- in the user code before any templates are applied).
teaserField :: String           -- ^ Key to use
            -> Snapshot         -- ^ Snapshot to load
            -> Context a        -- ^ Resulting context
teaserField = teaserFieldWithSeparator teaserSeparator

--------------------------------------------------------------------------------
-- | A context with "teaser" key which contain a teaser of the item, defined as
-- the snapshot content before the teaser separator. The item is loaded from the
-- given snapshot (which should be saved in the user code before any templates
-- are applied).
teaserFieldWithSeparator :: String           -- ^ Separator to use
                         -> String           -- ^ Key to use
                         -> Snapshot         -- ^ Snapshot to load
                         -> Context a        -- ^ Resulting context
teaserFieldWithSeparator separator key snapshot = stringField key $ \item -> do
    body <- itemBody <$> loadSnapshot (itemIdentifier item) snapshot
    case needlePrefix separator body of
        Nothing -> fail $
            "Hakyll.Web.Template.Context: no teaser defined for " ++
            show (itemIdentifier item)
        Just t -> return t

teaserSeparator :: String
teaserSeparator = "<!--more-->"

-- | Load a metadata using the dot notation
metadataIdField :: Identifier -> Context a
metadataIdField id = Context $ \key item -> do
  meta <- getMetadata id
  metadataJSON meta id key

-- | Load a metadata using the dot notation
metadataField :: Context a
metadataField = Context $ \key item -> do
  let id = itemIdentifier item
  meta <- getMetadata id
  metadataJSON meta id key

-- | Transform an JSON object to a Context
metadataJSON :: Object -> Identifier -> String -> Compiler ContextField
metadataJSON = metadataJSON' "."

metadataJSON' :: Text -> Object -> Identifier -> String -> Compiler ContextField
metadataJSON' sep obj id k = 
    let field = lookupDot' sep obj $ T.pack k 
        empty = noResult $ "No field '" ++ k ++ "' in metadata of item " ++ show id
    in maybe empty (fromValue id k) field

fromValue :: Identifier -> String -> Value -> Compiler ContextField
fromValue id key = \case
    Object _ -> noResult $ "Field '" ++ key ++ "' is an object in metadata of item " ++ show id
    x        -> return x
--------------------------------------------------------------------------------
-- Query a JSON object using the dot syntax
--------------------------------------------------------------------------------

-- | Lookup for a key with the dot notation `xxx.yyy` 
lookupDot :: Object -> Text -> Maybe Value
lookupDot = lookupDot' "."

-- | Lookup for a key with some seperator like the dot notation `xxx.yyy` 
lookupDot' :: Text -> Object -> Text -> Maybe Value
lookupDot' sep obj key = lookup obj $ T.splitOn sep key

-- | Lookup a JSON object recursively
lookup :: Object -> [Text] -> Maybe Value
lookup obj []     = Nothing
lookup obj [x]    = HM.lookup x obj
lookup obj (x:xs) = HM.lookup x obj >>= \case
  Object obj' -> lookup obj' xs
  _           -> Nothing

fieldType :: ContextField -> String
fieldType = \case
  String _ -> "string"
  Number _ -> "number"
  Array  _ -> "list"
  Object _ -> "object"
  Bool   _ -> "bool"
  Null     -> "empty"