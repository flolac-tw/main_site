{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}   
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE PatternSynonyms            #-}   

module Hakyll.Web.ExtendedTemplate.Type where

import           Control.Applicative     ((<|>))
import           Data.Binary             (Binary, get, getWord8, put, putWord8)
import           Data.List               (intercalate)
import           Data.Yaml               (Value(..), Object)
import           Data.Text               (Text)
import           Data.Typeable           (Typeable)

import           GHC.Exts                (IsString (..))
import           GHC.Generics            (Generic)

import           Hakyll.Core.Item
import           Hakyll.Core.Compiler
import           Hakyll.Core.Writable

type TemplateKey = String

--------------------------------------------------------------------------------
-- | Elements of a template.
data TemplateElement
    = Chunk String
    | Expr TemplateExpr
    | Escaped
      -- expr, then, else
    | If TemplateBoolExpr [TemplateElement] (Maybe [TemplateElement])
      -- expr, body, separator
    | ForEach TemplateKey TemplateExpr [TemplateElement] (Maybe [TemplateElement])
      -- filename
    | TrimL
    | TrimR
    deriving (Show, Eq, Typeable)

--------------------------------------------------------------------------------
instance Binary TemplateElement where
    put (Chunk string) = putWord8 0 >> put string
    put (Expr e)       = putWord8 1 >> put e
    put  Escaped       = putWord8 2
    put (If e t f)     = putWord8 3 >> put e >> put t >> put f
    put (ForEach e f b s) = putWord8 4 >> put e >> put f >> put b >> put s
    put  TrimL         = putWord8 5
    put  TrimR         = putWord8 6

    get = getWord8 >>= \tag -> case tag of
        0 -> Chunk <$> get
        1 -> Expr <$> get
        2 -> pure Escaped
        3 -> If <$> get <*> get <*> get
        4 -> ForEach <$> get <*> get <*> get <*> get
        5 -> pure TrimL
        6 -> pure TrimR
        _ -> error "Hakyll.Web.Template.Internal: Error reading cached template"

--------------------------------------------------------------------------------
-- | Expression in a template
data TemplateExpr
    = Ident TemplateKey
    | StringLiteral String
    deriving (Eq, Typeable)

--------------------------------------------------------------------------------
-- | Value expression used in boolean conditions.
data TemplateValueExpr
    = VIdent TemplateKey
    | VStringLiteral String
    | VNumberLiteral Integer
    deriving (Eq, Typeable)

--------------------------------------------------------------------------------
data TemplateCompOp
    = OpEq
    | OpNeq
    | OpLt
    | OpLte
    | OpGt
    | OpGte
    deriving (Eq, Typeable)

--------------------------------------------------------------------------------
-- | Boolean expression used in @$if()$@.
data TemplateBoolExpr
    = BoolNot TemplateBoolExpr
    | BoolAnd TemplateBoolExpr TemplateBoolExpr
    | BoolOr  TemplateBoolExpr TemplateBoolExpr
    | BoolCompare TemplateCompOp TemplateValueExpr TemplateValueExpr
    | BoolTruthy TemplateValueExpr
    deriving (Eq, Typeable)

--------------------------------------------------------------------------------
instance Show TemplateExpr where
    show (Ident k)   = k
    show (StringLiteral s)         = show s

--------------------------------------------------------------------------------
instance Show TemplateValueExpr where
    show (VIdent k)         = k
    show (VStringLiteral s) = show s
    show (VNumberLiteral n) = show n

--------------------------------------------------------------------------------
instance Show TemplateCompOp where
    show OpEq  = "=="
    show OpNeq = "!="
    show OpLt  = "<"
    show OpLte = "<="
    show OpGt  = ">"
    show OpGte = ">="

--------------------------------------------------------------------------------
instance Show TemplateBoolExpr where
    show (BoolNot e)               = "!" ++ show e
    show (BoolAnd a b)             = show a ++ " && " ++ show b
    show (BoolOr a b)              = show a ++ " || " ++ show b
    show (BoolCompare op a b)      = show a ++ " " ++ show op ++ " " ++ show b
    show (BoolTruthy v)            = show v

--------------------------------------------------------------------------------
instance Binary TemplateExpr where
    put (Ident k)         = putWord8 0 >> put k
    put (StringLiteral s) = putWord8 2 >> put s

    get = getWord8 >>= \case
        0 -> Ident         <$> get
        2 -> StringLiteral <$> get
        _ -> error "Hakyll.Web.Template.Internal: Error reading cached template"

--------------------------------------------------------------------------------
instance Binary TemplateValueExpr where
    put (VIdent k)         = putWord8 0 >> put k
    put (VStringLiteral s) = putWord8 1 >> put s
    put (VNumberLiteral n) = putWord8 2 >> put n

    get = getWord8 >>= \case
        0 -> VIdent         <$> get
        1 -> VStringLiteral <$> get
        2 -> VNumberLiteral <$> get
        _ -> error "Hakyll.Web.Template.Internal: Error reading cached template"

--------------------------------------------------------------------------------
instance Binary TemplateCompOp where
    put OpEq  = putWord8 0
    put OpNeq = putWord8 1
    put OpLt  = putWord8 2
    put OpLte = putWord8 3
    put OpGt  = putWord8 4
    put OpGte = putWord8 5

    get = getWord8 >>= \case
        0 -> pure OpEq
        1 -> pure OpNeq
        2 -> pure OpLt
        3 -> pure OpLte
        4 -> pure OpGt
        5 -> pure OpGte
        _ -> error "Hakyll.Web.Template.Internal: Error reading cached template"

--------------------------------------------------------------------------------
instance Binary TemplateBoolExpr where
    put (BoolNot e)          = putWord8 0 >> put e
    put (BoolAnd a b)        = putWord8 1 >> put a >> put b
    put (BoolOr a b)         = putWord8 2 >> put a >> put b
    put (BoolCompare o a b)  = putWord8 3 >> put o >> put a >> put b
    put (BoolTruthy v)       = putWord8 4 >> put v

    get = getWord8 >>= \case
        0 -> BoolNot      <$> get
        1 -> BoolAnd      <$> get <*> get
        2 -> BoolOr       <$> get <*> get
        3 -> BoolCompare  <$> get <*> get <*> get
        4 -> BoolTruthy   <$> get
        _ -> error "Hakyll.Web.Template.Internal: Error reading cached template"

--------------------------------------------------------------------------------
-- | Datatype used for template substitutions.
data Template = Template
    { tplElements :: [TemplateElement]
    , tplOrigin   :: FilePath  -- Only for error messages.
    } deriving (Show, Eq, Generic, Binary, Typeable)

--------------------------------------------------------------------------------
instance Writable Template where
    -- Writing a template is impossible
    write _ _ = return ()

--------------------------------------------------------------------------------
-- | The 'Context' monoid. Please note that the order in which you
-- compose the items is important. For example in
--
-- > field "A" f1 <> field "A" f2
--
-- the first context will overwrite the second.
--
-- @
-- 'metadataField' \<\> field \"date\" fDate
-- @
--
type ContextField = Value
newtype Context a 
  = Context { getContext :: String -> Item a -> Compiler ContextField }

instance Semigroup (Context a) where 
    (<>) (Context f) (Context g) = Context $ \key item ->
        f key item <|> g key item
