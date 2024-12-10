{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}

module Parser.ParserImpl where

import AST
import Parser.Lexer
import Data.Void            ( Void )
import Control.Monad.Reader
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import qualified Data.Text as T
import qualified Data.Map.Strict        as M


-- Network parser.
pNetwork :: Parser Network
pNetwork = Network <$> pImports <*> pLanguage <*> pExpressionsTop

-- Parse import statements.
pImports :: Parser [Import]
pImports = do
    imports <- sepBy pImport semicolon
    _ <- dot
    return imports
    <|>
    return []

-- Parse single import statement.
pImport :: Parser Import
pImport = do
    _ <- pString "import"
    Imp <$> filename

-- Parse language statements
pLanguage :: Parser Language
pLanguage = Language <$> pLangDef

-- Parse languge definition.
pLangDef :: Parser LanguageOptions
pLangDef = do
    language <- M.fromList <$> sepBy pLangOption semicolon
    _ <- dot
    return language
    <|> return M.empty

-- Parse aspect tag and language options.
pLangOption :: Parser (ATag, [ALang])
pLangOption = do
    _ <- pString "lang"
    aTag <- tag
    lang <- braces $ sepBy1 language comma
    return (aTag, lang)

-- Parse Expression statements. End with period.
pExpressionsTop :: Parser [Expression]
pExpressionsTop = try (do
    exps <- pExpressions
    _ <- dot 
    eof
    return exps)
    <|> do
    eof
    return []
    <|> 
    fail "Did not reach the end of file during parsing."

-- Parse Expression statements. End without period. Used within conditions. 
pExpressions :: Parser [Expression]
-- pExpressions = try (do
--     sepBy pExpression semicolon)
pExpressions = do
    sepBy pExpression semicolon
    <|> return []

pExpression :: Parser Expression
pExpression = choice [
        pPolicyTemplate
    ,   pWhen
    ,   pIf
    ,   pDelegation
    ,   pPredicate
    ,   pImplication
    ,   pGroup
    ,   pPolicy
    ,   pVariable]

pVariable :: Parser Expression
pVariable = EVar <$> variable

pIf :: Parser Expression
pIf = do
    pString "if";   r  <- parens relation
    pString "then"; e1 <- braces pExpressions
    pString "else"; e2 <- braces pExpressions
    return $ EIf r e1 e2

pWhen :: Parser Expression
pWhen = do
    pString "when";      r  <- parens relation
    pString "do";        e1 <- braces pExpressions
    pString "otherwise"; e2 <- braces pExpressions
    return $ EWhen r e1 e2

pImplication :: Parser Expression
pImplication = do
    pString "for";   v <- variable
    pString "where"; p <- braces preds
    pString "do";    e <- braces pExpressions
    return $ EImp v p e

pDelegation :: Parser Expression
pDelegation = do
    pString "trust"; symbol "(";
    from <- userOrVariable;    comma
    to <- userOrVariable;      symbol ")"
    pString "with"
    EDel from to <$> (pPolicy <|> pVariable)

pPolicyTemplate :: Parser Expression
pPolicyTemplate = do
    pString "policy"
    name <- variable; equal
    EPolTmp name <$> pPolicy

pGroup :: Parser Expression
pGroup = do
    pString "group"; name    <- variable
    equal;           members <- brackets $ sepBy1 user comma
    return $ EGroup name members

pPredicate :: Parser Expression
pPredicate = try (do
    pString "group"; binding  <- variable; equal
    pString "pred";  variable <- variable 
    pString "in";    p        <- braces preds
    return $ EPred binding variable p)