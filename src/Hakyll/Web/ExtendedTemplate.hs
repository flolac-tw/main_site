{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
{-# LANGUAGE ScopedTypeVariables           #-}
{-# LANGUAGE LambdaCase                    #-}
{-# LANGUAGE OverloadedStrings             #-}

module Hakyll.Web.ExtendedTemplate
    ( Template (..)
    , template
    , templateBodyCompiler
    , templateCompiler
    , applyTemplate
    , loadAndApplyTemplate
    , applyAsTemplate
    , compileTemplateItem
    , compileTemplateFile
    , trim
    , module Hakyll.Web.ExtendedTemplate.Context 
    ) where

--------------------------------------------------------------------------------
import           Control.Monad
import           Control.Monad.Except                 (catchError)

import qualified Data.HashMap.Strict                  as HM
import qualified Data.Text                            as T
import           Data.Yaml                            (Value(..))
import           Data.List
import qualified Data.List.NonEmpty                   as NonEmpty
import           Data.Vector                          (Vector)
import qualified Data.Vector                          as V

import           Hakyll.Core.Compiler
import           Hakyll.Core.Compiler.Internal
import           Hakyll.Core.Identifier
import           Hakyll.Core.Item
import           Hakyll.Core.Writable

import           Hakyll.Web.ExtendedTemplate.Context
import           Hakyll.Web.ExtendedTemplate.Type    (Template(..), ContextField(..))
import           Hakyll.Web.ExtendedTemplate.Trim
import           Hakyll.Web.ExtendedTemplate.Parser

-- | Wrap the constructor to ensure trim is called.
template :: FilePath -> [TemplateElement] -> Template
template p = flip Template p . trim

--------------------------------------------------------------------------------
-- | Parse an item body into a template.
-- Provides useful error messages in the 'Compiler' monad.
compileTemplateItem :: Item String -> Compiler Template
compileTemplateItem item = let file = itemIdentifier item
                           in compileTemplateFile file (itemBody item)

--------------------------------------------------------------------------------
compileTemplateFile :: Identifier -> String -> Compiler Template
compileTemplateFile file = either fail (return . template origin)
                         . parseTemplateElemsFile origin
  where
    origin = show file

--------------------------------------------------------------------------------
-- | Read a template, without metadata header
templateBodyCompiler :: Compiler (Item Template)
templateBodyCompiler = cached "Hakyll.Web.Template.templateBodyCompiler" $ do
    item <- getResourceBody
    file <- getUnderlying
    withItemBody (compileTemplateFile file) item

--------------------------------------------------------------------------------
-- | Read complete file contents as a template
templateCompiler :: Compiler (Item Template)
templateCompiler = cached "Hakyll.Web.Template.templateCompiler" $ do
    item <- getResourceString
    file <- getUnderlying
    withItemBody (compileTemplateFile file) item

--------------------------------------------------------------------------------
-- | Interpolate template expressions from ctx values in a page
applyTemplate :: Template                -- ^ Template
              -> Context a               -- ^ Context
              -> Item a                  -- ^ Page
              -> Compiler (Item String)  -- ^ Resulting item
applyTemplate tpl ctx item = do
    body <- applyTemplate' (tplElements tpl) ctx item `catchError` handler
    return $ itemSetBody body item
  where
    tplName = tplOrigin tpl
    itemName = show $ itemIdentifier item
    handler es = fail $ "Hakyll.Web.Template.applyTemplate: Failed to " ++
        (if tplName == itemName
          then "interpolate template in item " ++ itemName
          else "apply template " ++ tplName ++ " to item " ++ itemName) ++
        ":\n" ++ intercalate ",\n" es

--------------------------------------------------------------------------------
applyTemplate'
    :: forall a.
       [TemplateElement] -- ^ Unwrapped Template
    -> Context a         -- ^ Context
    -> Item a            -- ^ Page
    -> Compiler String   -- ^ Resulting item
applyTemplate' tmps ctx item = go tmps
  where
    ctx' :: String -> Item a -> Compiler ContextField
    ctx' = getContext (ctx <> missingField)

    go = fmap concat . mapM applyElem

    applyElem :: TemplateElement -> Compiler String

    applyElem TrimL = trimError
    applyElem TrimR = trimError
    applyElem (Chunk c) = return c
    applyElem (Expr e) = withErrorMessage evalMsg (applyStringExpr typeMsg e)
      where
        evalMsg = "In expr '$" ++ show e ++ "$'"
        typeMsg = "expr '$" ++ show e ++ "$'"
    applyElem Escaped = return "$"
    applyElem (If e t mf) = compilerTry (applyExpr e) >>= handle
      where
        f = maybe (return "") go mf
        handle (Right _)                      = go t
        handle (Left (CompilationNoResult _)) = f
        handle (Left (CompilationFailure es)) = debug (NonEmpty.toList es) >> f
        debug = compilerDebugEntries ("Hakyll.Web.Template.applyTemplate: " ++
            "[ERROR] in 'if' condition on expr '" ++ show e ++ "':")
    applyElem (ForEach iter exp body sep) = 
      withErrorMessage headMsg (applyExpr exp) >>= \case
        Array xs -> withErrorMessage bodyMsg $ do
          sep <- maybe (return "") go sep
          bs  <- forM (V.toList xs) $ \val -> do
            let id  = itemIdentifier item
                obj = HM.singleton (T.pack iter) val
                cxt = (Context $ \key _ -> metadataJSON obj id key) <> ctx
            applyTemplate' body cxt item
          return $ intercalate sep bs
        field         -> expected "list" (fieldType field) typeMsg
      where
        headMsg = "In expr '$foreach(" ++ show iter ++ ")in(" ++ show exp ++ ")$'"
        typeMsg = "loop expr '" ++ show exp ++ "'"
        bodyMsg = "In loop ctx of '$foreach(" ++ show iter ++ ")in(" ++ show exp ++ ")$'"

    applyExpr :: TemplateExpr -> Compiler ContextField
    applyExpr (StringLiteral s) = return (String $ T.pack s)
    applyExpr (Ident k) = ctx' k item
    ----------------------------------------------------------------------------

    applyStringExpr :: String -> TemplateExpr -> Compiler String
    applyStringExpr msg expr =
        applyExpr expr >>= \case
          String s -> return $ T.unpack s
          field    -> expected "string" (fieldType field) msg

    expected typ act expr = fail $ unwords ["Hakyll.Web.Template.applyTemplate:",
        "expected", typ, "but got", act, "for", expr]

        -- expected to never happen with all templates constructed by 'template'
    trimError = fail $ "Hakyll.Web.Template.applyTemplate: template not fully trimmed."

--------------------------------------------------------------------------------
-- | The following pattern is so common:
--
-- > tpl <- loadBody "templates/foo.html"
-- > someCompiler
-- >     >>= applyTemplate tpl ctx
--
-- That we have a single function which does this:
--
-- > someCompiler
-- >     >>= loadAndApplyTemplate "templates/foo.html" ctx
loadAndApplyTemplate :: Identifier              -- ^ Template identifier
                     -> Context a               -- ^ Context
                     -> Item a                  -- ^ Page
                     -> Compiler (Item String)  -- ^ Resulting item
loadAndApplyTemplate identifier ctx item = do
    tpl <- loadBody identifier
    applyTemplate tpl ctx item

--------------------------------------------------------------------------------
-- | It is also possible that you want to substitute @$key$@s within the body of
-- an item. This function does that by interpreting the item body as a template,
-- and then applying it to itself.
applyAsTemplate :: Context String          -- ^ Context
                -> Item String             -- ^ Item and template
                -> Compiler (Item String)  -- ^ Resulting item
applyAsTemplate ctx item = do
    tpl <- compileTemplateItem item
    applyTemplate tpl ctx item

