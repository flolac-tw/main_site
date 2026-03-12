--------------------------------------------------------------------------------
-- | Module containing the elements used in a template.  A template is generally
-- just a list of these elements.
module Hakyll.Web.ExtendedTemplate.Parser
    ( TemplateKey (..)
    , TemplateExpr (..)
    , TemplateElement (..)
    , templateElems
    , parseTemplateElemsFile
    ) where

--------------------------------------------------------------------------------
import           Control.Applicative     ((<|>), (<*))
import           Control.Monad           (void, guard, mzero)
import           Control.Arrow           (left)
import           Data.List               (intercalate)
import           Data.Maybe              (isJust)
import qualified Text.Parsec             as P
import qualified Text.Parsec.String      as P

import           Hakyll.Web.ExtendedTemplate.Type

parseTemplateElemsFile :: FilePath -> String -> Either String [TemplateElement]
parseTemplateElemsFile file = left (\e -> "Cannot parse template " ++ show e)
                            . P.parse (templateElems <* P.eof) file

templateElems :: P.Parser [TemplateElement]
templateElems = mconcat <$> P.many (P.choice [ lift chunk
                                             , lift escaped
                                             , conditional
                                             , foreach
                                             , expr
                                             ])
    where lift = fmap (:[])

chunk :: P.Parser TemplateElement
chunk = Chunk <$> P.many1 (P.noneOf "$")

expr :: P.Parser [TemplateElement]
expr = P.try $ do
    trimLExpr <- trimOpen
    e <- expr'
    trimRExpr <- trimClose
    return $ [TrimL | trimLExpr] ++ [Expr e] ++ [TrimR | trimRExpr]

expr' :: P.Parser TemplateExpr
expr' = stringLiteral <|> ident

escaped :: P.Parser TemplateElement
escaped = Escaped <$ P.try (P.string "$$")

trimOpen :: P.Parser Bool
trimOpen = do
    void $ P.char '$'
    trimLIf <- P.optionMaybe $ P.try (P.char '-')
    pure $ isJust trimLIf

trimClose :: P.Parser Bool
trimClose = do
    trimIfR <- P.optionMaybe $ P.char '-'
    void $ P.char '$'
    pure $ isJust trimIfR

conditional :: P.Parser [TemplateElement]
conditional = P.try $ do
    -- if
    trimLIf <- trimOpen
    void $ P.string "if("
    e <- boolExpr
    void $ P.char ')'
    trimRIf <- trimClose
    -- then
    thenBranch <- templateElems
    -- else
    elseParse <- opt "else"
    -- endif
    trimLEnd <- trimOpen
    void $ P.string "endif"
    trimREnd <- trimClose

    -- As else is optional we need to sort out where any Trim_s need to go.
    let (thenBody, elseBody) = maybe (thenNoElse, Nothing) thenElse elseParse
            where thenNoElse =
                      [TrimR | trimRIf] ++ thenBranch ++ [TrimL | trimLEnd]

                  thenElse (trimLElse, elseBranch, trimRElse) = (thenB, elseB)
                      where thenB = [TrimR | trimRIf]
                                 ++ thenBranch
                                 ++ [TrimL | trimLElse]

                            elseB = Just $ [TrimR | trimRElse]
                                        ++ elseBranch
                                        ++ [TrimL | trimLEnd]

    pure $ [TrimL | trimLIf] ++ [If e thenBody elseBody] ++ [TrimR | trimREnd]

foreach :: P.Parser [TemplateElement]
foreach = P.try $ do
    -- foreach 
    trimLFor <- trimOpen
    void $ P.string "foreach("
    Ident it <- ident
    void $ P.string ")in("
    e <- expr'
    void $ P.char ')'
    trimRFor <- trimClose
    -- body
    bodyBranch <- templateElems
    -- sep
    sepParse <- opt "sep"
    -- endfor
    trimLEnd <- trimOpen
    void $ P.string "endforeach"
    trimREnd <- trimClose

    -- As sep is optional we need to sort out where any Trim_s need to go.
    let (forBody, sepBody) = maybe (forNoSep, Nothing) forSep sepParse
            where forNoSep =
                      [TrimR | trimRFor] ++ bodyBranch ++ [TrimL | trimLEnd]

                  forSep (trimLSep, sepBranch, trimRSep) = (forB, sepB)
                      where forB = [TrimR | trimRFor]
                                ++ bodyBranch
                                ++ [TrimL | trimLSep]

                            sepB = Just $ [TrimR | trimRSep]
                                       ++ sepBranch
                                       ++ [TrimL | trimLEnd]

    pure $ [TrimL | trimLFor] ++ [ForEach it e forBody sepBody] ++ [TrimR | trimREnd]

ident :: P.Parser TemplateExpr
ident = P.try $ Ident <$> key

stringLiteral :: P.Parser TemplateExpr
stringLiteral = StringLiteral <$> stringLiteralRaw

--------------------------------------------------------------------------------
-- Boolean expressions for if()
--------------------------------------------------------------------------------

boolExpr :: P.Parser TemplateBoolExpr
boolExpr = spaces *> boolOr <* spaces

boolOr :: P.Parser TemplateBoolExpr
boolOr = boolAnd `P.chainl1` (symbol "||" *> pure BoolOr)

boolAnd :: P.Parser TemplateBoolExpr
boolAnd = boolNot `P.chainl1` (symbol "&&" *> pure BoolAnd)

boolNot :: P.Parser TemplateBoolExpr
boolNot = (symbol "!" *> (BoolNot <$> boolNot)) <|> boolTerm

boolTerm :: P.Parser TemplateBoolExpr
boolTerm = parens boolExpr <|> P.try boolCompare <|> boolTruthy

boolCompare :: P.Parser TemplateBoolExpr
boolCompare = do
    lhs <- valueExpr
    op  <- compOp
    rhs <- valueExpr
    pure $ BoolCompare op lhs rhs

boolTruthy :: P.Parser TemplateBoolExpr
boolTruthy = BoolTruthy <$> valueExpr

valueExpr :: P.Parser TemplateValueExpr
valueExpr = lexeme (numberLiteral <|> stringLiteralValue <|> identValue)

identValue :: P.Parser TemplateValueExpr
identValue = VIdent <$> key

stringLiteralValue :: P.Parser TemplateValueExpr
stringLiteralValue = VStringLiteral <$> stringLiteralRaw

numberLiteral :: P.Parser TemplateValueExpr
numberLiteral = P.try $ do
    sign <- P.optionMaybe (P.char '-')
    digits <- P.many1 P.digit
    let n = read digits :: Integer
    pure $ VNumberLiteral $ case sign of
        Just _  -> negate n
        Nothing -> n

compOp :: P.Parser TemplateCompOp
compOp =
    P.choice
        [ P.try (symbol "==") *> pure OpEq
        , P.try (symbol "!=") *> pure OpNeq
        , P.try (symbol "<=") *> pure OpLte
        , P.try (symbol ">=") *> pure OpGte
        , symbol "<"  *> pure OpLt
        , symbol ">"  *> pure OpGt
        ]

parens :: P.Parser a -> P.Parser a
parens p = symbol "(" *> p <* symbol ")"

symbol :: String -> P.Parser String
symbol s = lexeme (P.string s)

lexeme :: P.Parser a -> P.Parser a
lexeme p = p <* spaces

spaces :: P.Parser ()
spaces = P.skipMany (P.oneOf " \t\r\n")

stringLiteralRaw :: P.Parser String
stringLiteralRaw = do
    void $ P.char '\"'
    str <- P.many $ do
        x <- P.noneOf "\""
        if x == '\\' then P.anyChar else return x
    void $ P.char '\"'
    return str

opt :: String -> P.Parser (Maybe (Bool, [TemplateElement], Bool))
opt clause = P.optionMaybe $ P.try $ do
    trimL <- trimOpen
    void $ P.string clause
    trimR <- trimClose
    branch <- templateElems
    pure (trimL, branch, trimR)

key :: P.Parser String
key = do
    -- Ensure trailing '-' binds to '$' if present.
    let hyphon = P.try $ do
            void $ P.char '-'
            x <- P.lookAhead P.anyChar
            guard $ x /= '$'
            pure '-'
    i <- (:) <$> P.letter <*> P.many (P.alphaNum <|> P.oneOf "_." <|> hyphon)
    if i `elem` reservedKeys then mzero else return i

reservedKeys :: [String]
reservedKeys =
    [ "in"
    , "if"
    , "else"
    , "endif"
    , "foreach"
    , "sep"
    , "endforeach"
    ]
