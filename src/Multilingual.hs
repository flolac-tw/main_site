module Multilingual where

import           Control.Monad
import           Data.Monoid
import           Data.List

import           Hakyll

import           Metadata

loadAndApplyTemplatesLC lc ctx ids it =
    foldM (\item tpl -> loadAndApplyTemplateLC tpl lc ctx item) it ids

loadAndApplyTemplateLC :: Identifier -> String -> Context a -> Item a -> Compiler (Item String)
loadAndApplyTemplateLC id lc context item =
    let locale = redirectCtx "LC." (lc ++ ".") $ metaIdFields id
        ctx    = constField "lang" lc <> locale <> context
    in loadAndApplyTemplate id ctx item

localeCtx :: String -> Context a
localeCtx lc =
    let intLC  = metadataFields'
        locale = redirectCtx "LC." (lc ++ ".") intLC
    in constField "lang" lc <> locale
------------------------------------------------------------------------------

-- | Context redirect $LC.title$ => $en.title$ or $zh.title$ ...
redirectCtx :: String -> String -> Context a -> Context a
redirectCtx origin after (Context f) = Context $ \k a i ->
    case origin `stripPrefix` k of
        Just k'   -> f (after ++ k') a i
        Nothing   -> f k a i