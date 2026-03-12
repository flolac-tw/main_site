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

import           Data.Aeson.Key                       (fromString)
import qualified Data.Aeson.KeyMap                    as KM
import qualified Data.Text                            as T
import           Data.Yaml                            (Value(..))
import           Data.List
import qualified Data.List.NonEmpty                   as NonEmpty
import           Data.Maybe                           (isJust)
import           Data.Vector                          (Vector)
import qualified Data.Vector                          as V

import           Hakyll.Core.Compiler
import           Hakyll.Core.Compiler.Internal
import           Hakyll.Core.Identifier
import           Hakyll.Core.Item
import           Hakyll.Core.Writable

import           Hakyll.Web.ExtendedTemplate.Context
import           Hakyll.Web.ExtendedTemplate.Type    (Template(..), ContextField(..), TemplateBoolExpr(..), TemplateCompOp(..), TemplateValueExpr(..))
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
    applyElem (If e t mf) = compilerTry (applyBoolExpr e) >>= handle
      where
        f = maybe (return "") go mf
        handle (Right True)                   = go t
        handle (Right False)                  = f
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
                obj = KM.singleton (fromString iter) val
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

    ----------------------------------------------------------------------------
    -- Boolean expressions for if()
    ----------------------------------------------------------------------------

    applyBoolExpr :: TemplateBoolExpr -> Compiler Bool
    applyBoolExpr = \case
        BoolNot e -> not <$> applyBoolExpr e
        BoolAnd a b -> do
            a' <- applyBoolExpr a
            if a' then applyBoolExpr b else return False
        BoolOr a b -> do
            a' <- applyBoolExpr a
            if a' then return True else applyBoolExpr b
        BoolCompare op a b -> do
            ma <- evalValue a
            case ma of
                Nothing -> return False
                Just va -> do
                    mb <- evalValue b
                    case mb of
                        Nothing -> return False
                        Just vb -> compareValues op va vb
        BoolTruthy v -> do
            mv <- evalValue v
            return $ isJust mv

    evalValue :: TemplateValueExpr -> Compiler (Maybe ContextField)
    evalValue = \case
        VStringLiteral s -> return . Just . String $ T.pack s
        VNumberLiteral n -> return . Just . Number $ fromInteger n
        VIdent k -> compilerTry (ctx' k item) >>= \case
            Right v                     -> return (Just v)
            Left (CompilationNoResult _) -> return Nothing
            Left (CompilationFailure es) -> do
                compilerDebugEntries
                    ("Hakyll.Web.Template.applyTemplate: " ++
                     "[ERROR] in 'if' value on expr '" ++ k ++ "':")
                    (NonEmpty.toList es)
                return Nothing

    compareValues :: TemplateCompOp -> ContextField -> ContextField -> Compiler Bool
    compareValues op a b = case (a, b) of
        (String l, String r) -> return $ compareOrd op l r
        (Number l, Number r) -> return $ compareOrd op l r
        (Bool l, Bool r)     -> compareBool op l r
        (Null, Null)         -> compareNull op
        _ -> fail $ unwords
            [ "Hakyll.Web.Template.applyTemplate: cannot compare"
            , fieldType a, "and", fieldType b
            , "with", show op
            ]

    compareOrd :: Ord b => TemplateCompOp -> b -> b -> Bool
    compareOrd o l r = case o of
        OpEq  -> l == r
        OpNeq -> l /= r
        OpLt  -> l <  r
        OpLte -> l <= r
        OpGt  -> l >  r
        OpGte -> l >= r

    compareBool :: TemplateCompOp -> Bool -> Bool -> Compiler Bool
    compareBool o l r = case o of
        OpEq  -> return (l == r)
        OpNeq -> return (l /= r)
        _     -> fail $ "Hakyll.Web.Template.applyTemplate: bools only support == and !="

    compareNull :: TemplateCompOp -> Compiler Bool
    compareNull o = case o of
        OpEq  -> return True
        OpNeq -> return False
        _     -> fail $ "Hakyll.Web.Template.applyTemplate: nulls only support == and !="

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
