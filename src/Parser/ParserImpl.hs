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
import GHC.Base (undefined)
import qualified Data.Map.Strict        as M


pNetwork :: Parser Network
-- pNetwork = Network <$> pImports <*> pLanguage
pNetwork = Network <$> pImports <*> pLanguage <*> pExpressions


-- Parse import statements
pImports :: Parser [Import]
pImports = do
    imports <- sepBy pImport semicolon
    _ <- dot
    return imports
    <|>
    return []

pImport :: Parser Import
pImport = do
    _ <- pString "import"
    Imp <$> filename

-- Parse language statements
pLanguage :: Parser Language
pLanguage = Language <$> pLangDef

-- pLangDef :: Parser [Aspects]
-- pLangDef = do
--     language <- sepBy aspect semicolon
--     _ <- dot
--     return language
--     <|> return []

-- aspect :: Parser Aspects
-- aspect = do
--     _ <- pString "lang"
--     atag <- tag 
--     lang <- braces $ sepBy1 language comma
--     return $ Aspect atag lang


pLangDef :: Parser LanguageOptions
pLangDef = do 
    language <- M.fromList <$> sepBy pLangOption semicolon
    _ <- dot 
    return language
    <|> return M.empty

pLangOption :: Parser (ATag, [ALang])
pLangOption = do
    _ <- string "lang"
    aTag <- tag
    lang <- braces $ sepBy1 language comma
    return (aTag, lang)


-- Parse Expression statements
pExpressions :: Parser [Expression]
pExpressions = try (do
    exps <- sepBy pExpression semicolon
    _ <- dot
    return exps)
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


-- MISSING: Can not be in keywords 
pVariable :: Parser Expression
pVariable = EVar <$> atom

pIf :: Parser Expression
pIf = do
    pString "if";  r  <- parens relation
    pString "then"; e1 <- braces pExpressions
    pString "else"; e2 <- braces pExpressions
    return $ EIf r e1 e2

pWhen :: Parser Expression
pWhen = do
    pString "when";      r  <- relation
    pString "do";        e1 <- braces pExpressions
    pString "otherwise"; e2 <- braces pExpressions
    return $ EWhen r e1 e2

pPredicate :: Parser Expression
pPredicate = do
    pString "pred"; name <- atom -- Might need something else for this
    pString "in";   p    <- braces preds
    return $ EPred name p

pImplication :: Parser Expression
pImplication = do
    pString "for";   v <- parens atom
    pString "where"; r <- relation
    pString "do";    e <- braces pExpressions
    return $ EImp v r e 

pDelegation :: Parser Expression
-- pDelegation = do
--     pString "trust"; symbol "("; 
--     from <- atom; comma
--     to <- atom
--     degree <- degree; symbol ")"
--     pString "with"
--     EDel from to degree <$> (pPolicy <|> pVariable)
pDelegation = do
    pString "trust"; symbol "("; 
    from <- atom;    comma
    to <- atom;      symbol ")"
    pString "with"
    EDel from to <$> (pPolicy <|> pVariable)

pPolicyTemplate :: Parser Expression
pPolicyTemplate = do
    pString "policy"
    name <- atom; equal
    EPolTmp name <$> pPolicy

pGroup :: Parser Expression
pGroup = do
    pString "group"; name    <- atom
    equal;           members <- brackets $ sepBy1 atom comma
    return $ EGroup name members

-- data Expression = 
--       EDel Atom Atom Degree Expression
--     | EIf Relation [Expression] [Expression]
--     | EWhen Relation [Expression] [Expression]
--     | EPolTmp Atom Expression
--     | EPol [Pol]
--     | EVar Atom
--     | EValue Int
--     | EImp Atom Relation [Expression] 
--     | EPred Atom [Pred]
--     | EGroup Atom [Atom]
--   deriving (Eq, Show)