{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase        #-}

module Metadata where

--import           Control.Applicative           (Alternative (..))
import qualified Data.HashMap.Strict            as HMS
import           Data.Text (Text)
import qualified Data.Text                      as T
import           Data.Yaml
--import qualified Data.Vector as V
import           Data.Scientific
import           Data.Time
import           Prelude hiding (lookup)

import           Hakyll
import           Hakyll.Core.Item
import           Hakyll.Core.Compiler

-- | Load the YAML preamble of a file as context
metaIdFields :: Identifier -> Context a
metaIdFields id = Context $ \k _ _ -> do
  meta <- getMetadata id
  metadataJSON meta id k

-- | Load a metadata using the dot notation
metadataFields' :: Context a
metadataFields' = Context $ \k _ item -> do
  let id = itemIdentifier item
  meta <- getMetadata id
  metadataJSON meta id k

metadataJSON :: Object -> Identifier -> String -> Compiler ContextField
metadataJSON obj id k = 
  let empty    = noResult $ "No '" ++ k ++ "' field in metadata " ++ "of item " ++ show id in
  flip (maybe empty) (lookupDot obj $ T.pack k) $ \case
    String t -> return $ StringField $ T.unpack t
    Number i -> return $ StringField $ if isInteger i 
      then formatScientific Fixed (Just 0) i 
      else show i
    Bool   b -> return $ StringField $ if b then "true" else "false"
    Array  _ -> noResult $ "Field" ++ k ++ "is an array in the metadata of item" ++ show id
    Object _ -> noResult $ "Field" ++ k ++ "is an object in the metadata of item" ++ show id

--------------------------------------------------------------------------------
-- Query a JSON object using the dot syntax
--------------------------------------------------------------------------------

-- | Lookup for a key with the dot notation `xxx.yyy` 
lookupDot :: Object -> Text -> Maybe Value
lookupDot obj = lookupDot' obj "."

-- | Lookup for a key with some seperator like the dot notation `xxx.yyy` 
lookupDot' :: Object -> Text -> Text -> Maybe Value
lookupDot' obj sep key = lookup obj (T.splitOn sep key)

-- | Lookup a JSON object recursively
lookup :: Object -> [Text] -> Maybe Value
lookup obj []     = Nothing
lookup obj [x]    = HMS.lookup x obj
lookup obj (x:xs) = HMS.lookup x obj >>= \case
  Object obj' -> lookup obj' xs
  _           -> Nothing