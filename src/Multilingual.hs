module Multilingual where

import           Control.Monad
import           Data.Monoid
import           Data.List

-- import           Hakyll hiding (Context)
import           Hakyll.Core.Identifier
import           Hakyll.Core.Item
import           Hakyll.Core.Compiler

import           Hakyll.Web.ExtendedTemplate

loadAndApplyTemplatesLC lc ctx ids it =
    foldM (\item tpl -> loadAndApplyTemplateLC tpl lc ctx item) it ids

loadAndApplyTemplateLC :: Identifier -> String -> Context a -> Item a -> Compiler (Item String)
loadAndApplyTemplateLC id lc context item =
    let locale = redirectCtx "LC." (lc ++ ".") $ metadataIdField id
        ctx    = stringField "lang" (const $ return lc) <> locale <> context
    in loadAndApplyTemplate id ctx item

localeCtx :: String -> Context a
localeCtx lc =
    let intLC  = metadataField
        locale = redirectCtx "LC." (lc ++ ".") metadataField
    in stringField "lang" (const $ return lc) <> locale
------------------------------------------------------------------------------

-- | Context redirect $LC.title$ => $en.title$ or $zh.title$ ...
redirectCtx :: String -> String -> Context a -> Context a
redirectCtx origin after (Context f) = Context $ \k i ->
    case origin `stripPrefix` k of
        Just k'   -> f (after ++ k') i
        Nothing   -> f k i