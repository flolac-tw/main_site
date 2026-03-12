module Multilingual where

import           Control.Monad
import           Data.List
import qualified Data.ByteString                 as B
import qualified Data.Yaml                       as Y
import           System.Directory                (doesFileExist)
import           System.FilePath                 (takeDirectory, (</>))

-- import           Hakyll hiding (Context)
import           Hakyll.Core.Compiler.Internal
import           Hakyll.Core.Dependencies
import           Hakyll.Core.Identifier
import           Hakyll.Core.Item
import           Hakyll.Core.Compiler

import           Hakyll.Web.ExtendedTemplate

loadAndApplyTemplatesLC lc ctx ids it =
    foldM (\item tpl -> loadAndApplyTemplateLC tpl lc ctx item) it ids

loadAndApplyTemplateLC :: Identifier -> String -> Context a -> Item a -> Compiler (Item String)
loadAndApplyTemplateLC id lc context item =
    let ctx = localize lc (metadataIdField id <> context)
    in loadAndApplyTemplate id ctx item

-- | Load i18n strings from <item>/i18n/<lc>.yaml with a global fallback.
i18nCtx :: String -> Context a
i18nCtx lc =
    let local  = relativeYamlCtx ("i18n/" ++ lc ++ ".yaml")
        global = yamlFileCtx ("content/i18n/" ++ lc ++ ".yaml")
    in prefixedCtx (lc ++ ".") (local <> global)

localeCtx :: String -> Context a
localeCtx lc =
    let base = metadataField <> configField "config.yaml" <> i18nCtx lc
    in localize lc base

-- | Add language fields and LC.* redirection to any context.
localize :: String -> Context a -> Context a
localize lc ctx =
    stringField "lang" (const $ return lc) <>
    redirectCtx "LC." (lc ++ ".") ctx

------------------------------------------------------------------------------

-- | Context redirect $LC.title$ => $en.title$ or $zh.title$ ...
redirectCtx :: String -> String -> Context a -> Context a
redirectCtx origin after (Context f) = Context $ \k i ->
    case origin `stripPrefix` k of
        Just k'   -> f (after ++ k') i
        Nothing   -> f k i

-- | Only respond to fields with the given prefix.
prefixedCtx :: String -> Context a -> Context a
prefixedCtx prefix (Context f) = Context $ \k i ->
    case prefix `stripPrefix` k of
        Just k'   -> f k' i
        Nothing   -> noResult $ "No field '" ++ k ++ "' in prefixed context."

-- | Load a YAML file as a context using dot notation.
yamlFileCtx :: FilePath -> Context a
yamlFileCtx fp = Context $ \k _ -> do
    compilerTellDependencies [IdentifierDependency (fromFilePath fp)]
    obj <- unsafeCompiler $ loadYamlObject fp
    metadataJSON obj (fromFilePath fp) k

-- | Load a YAML file relative to the item's directory (optional).
relativeYamlCtx :: FilePath -> Context a
relativeYamlCtx relPath = Context $ \k i -> do
    let baseDir = takeDirectory $ toFilePath (itemIdentifier i)
        fp = baseDir </> relPath
    exists <- unsafeCompiler $ doesFileExist fp
    if not exists
        then noResult $ "No i18n file at " ++ fp
        else do
            compilerTellDependencies [IdentifierDependency (fromFilePath fp)]
            obj <- unsafeCompiler $ loadYamlObject fp
            metadataJSON obj (fromFilePath fp) k

loadYamlObject :: FilePath -> IO Y.Object
loadYamlObject fp = do
    fileContent <- B.readFile fp
    let errOrVal = Y.decodeEither' fileContent
    case errOrVal of
        Left err -> fail $ "Failed to parse YAML: " ++ fp ++ " (" ++ show err ++ ")"
        Right (Y.Object obj) -> return obj
        Right _ -> fail $ "Expected YAML mapping at top-level: " ++ fp
