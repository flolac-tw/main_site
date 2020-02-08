{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase        #-}

module Multilingual where

import           Data.Monoid
import           Data.List
import qualified Data.HashMap.Strict            as HMS
import qualified Data.Text                      as T
import           Data.Yaml

import           Hakyll

------------------------------------------------------------------------------
loadAndApplyTemplateLC :: Identifier -> String -> Context a -> Item a -> Compiler (Item String)
loadAndApplyTemplateLC id lc context item =
    let locale = redirectCtx "LC." (lc ++ ".") $ metaFile (toFilePath id)
        ctx    = constField "lang" lc <> locale <> context
    in loadAndApplyTemplate id ctx item

applyLC :: String -> Item String -> Compiler (Item String)
applyLC lc =
    let intLC  = metadataFieldDot
        locale = redirectCtx "LC." (lc ++ ".") intLC
        ctx    = constField "lang" lc <> locale <> idCtx
    in applyAsTemplate ctx

------------------------------------------------------------------------------
-- Various contexts

-- $LC.title$ => $en.title$ or $zh.title$ ...
redirectCtx :: String -> String -> Context a -> Context a
redirectCtx origin after (Context f) = Context $ \k a i ->
    case origin `stripPrefix` k of
        Just k'   -> f (after ++ k') a i
        Nothing   -> f k a i

-- Every variable $xxx$ is substituted by $xxx$ itself.
idCtx :: Context a
idCtx = Context $ \k a i -> return $ StringField $ "$" ++ k ++ "$"

-- Load a file as metadata
metaFile :: FilePath -> Context a
metaFile fp = Context $ \k _ _ -> metaDot k $ fromFilePath fp

-- Load a metadata filed with dot accessor.
metadataFieldDot :: Context a
metadataFieldDot = Context $ \k _ -> metaDot k . itemIdentifier

metaDot :: String -> Identifier -> Compiler ContextField
metaDot k id = do
    let empty' = noResult $ "No '" ++ k ++ "' field in metadata " ++
            "of item " ++ show id
    meta <- getMetadata id
    maybe empty' (return . StringField) $ lookupStringDot k meta

-- Use `xxx.yyy` to access a member `yyy` of `xxx` in a key/value object.
lookupStringDot :: String -> Metadata -> Maybe String
lookupStringDot key = lookupStrings (splitBy '.' key)

lookupStrings :: [String] -> Metadata -> Maybe String
lookupStrings xs meta = case xs of
    []     -> Nothing
    [x]    -> lookupString x meta
    (x:xs) -> HMS.lookup (T.pack x) meta >>= \case
      Object meta' -> lookupStrings xs meta'
      _            -> Nothing

splitBy :: Eq a => a -> [a] -> [[a]]
splitBy _ [] = []
splitBy s xs = cons $ case break (== s) xs of
    (l, s') -> (l, case s' of
        []      -> []
        _:s''   -> splitBy s s'')
    where
        cons ~(h, t)        =  h : t

joinBy :: a -> [[a]] -> [a]
joinBy _ []     = []
joinBy _ [x]    = x
joinBy s (x:xs) = x ++ s : joinBy s xs

-- An ad hoc approach to change /xxx/yyy to /dom/yyy
substUpDom :: String -> String -> String
substUpDom dom = joinBy '/' . ([]:) . (dom:) . drop 2 . splitBy '/'

getURL :: Item a -> Compiler String
getURL i = do
    let id = itemIdentifier i
        empty' = fail $ "No route url found for item " ++ show id
    maybe empty' toUrl <$> getRoute id
