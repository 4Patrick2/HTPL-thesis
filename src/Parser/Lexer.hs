{-# LANGUAGE OverloadedStrings    #-}

module Parser.Lexer (module Parser.Lexer) where

import AST
import Data.Void            ( Void )
import Control.Monad.Reader
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import qualified Data.Text as T
import GHC.Base (undefined)

-- Parser monad
type Parser = ReaderT [T.Text] (Parsec Void T.Text)
-- type Parser = Parsec Void T.Text

------------------
---- Lexing 
------------------
spaceConsumer :: Parser ()
spaceConsumer = L.space space1 (L.skipLineComment "#") empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme spaceConsumer

symbol :: T.Text -> Parser T.Text
symbol = L.symbol spaceConsumer

integer :: Parser Int
integer = lexeme $ do--lexeme L.decimal
    i <- many digitChar
    return $ read i


parens, braces, brackets :: Parser a -> Parser a
parens    = between (symbol "(") (symbol ")")
braces    = between (symbol "{") (symbol "}")
brackets  = between (symbol "[") (symbol "]")

semicolon, comma, dot, colon :: Parser T.Text
semicolon = symbol ";"
comma     = symbol ","
dot       = symbol "."
colon     = symbol ":"

less, greater, equal :: Parser T.Text
less    = symbol "<"
greater = symbol ">"
equal   = symbol "="

pString :: String -> Parser T.Text
pString s = lexeme $ string (T.pack s)  

------------------

filename' :: Parser String
filename' = lexeme $ someTill alphaNumChar (string ".lan") <|> fail "Filename not properly formattet."

filename :: Parser T.Text
filename = lexeme $ T.pack <$> filename'

-- type LangDef  = M.Map ATag ALang
tag :: Parser ATag
tag = lexeme $ T.pack <$> tag'

tag' :: Parser String
tag' = (:) <$> upperChar <*> many alphaNumChar <|> fail "Aspect tag not properly formattet."

language :: Parser ALang
language = try (do
    f <- aLang
    s <- brackets aLang
    _ <- dot
    TDNS f s <$> aLang)
    <|> try (do
    f <- aLang
    s <- brackets aLang
    return $ TDNS f s (Degree Low))
    <|> try (do
    f <- aLang
    _ <- dot
    TDNS f (Degree Low) <$> aLang)
    <|> try (do
    f <- aLang
    return $ TDNS f (Degree Low) (Degree Low))
    <|> 
    fail "Language expression not well formed"

-- test[].test
-- test[]
-- test
-- test.test

aLang :: Parser Lang
aLang = do --lexeme $ do
    symbol "*"; return $ Degree High
    <|> do
    symbol "_"; return $ Degree Low
    <|> do
    Atom <$> atom

-- MISSING: Can not be in keywords
atom :: Parser T.Text
atom = lexeme $ T.pack <$> atom'

atom' :: Parser String 
atom' = (:) <$> letterChar <*> many alphaNumChar


preds :: Parser [Pred]
preds = sepBy1 predicate semicolon

predicate :: Parser Pred
predicate = do 
    from <- atom; comma
    to   <- atom; colon
    Pred from to <$> pPolicy

degree :: Parser Degree
degree = 
        try (do comma; symbol "*";     return High)
    <|> try (do comma; pString "high"; return High)
    <|> try (do comma; symbol "_";     return Low)
    <|> try (do comma; pString "low";  return Low)
    <|>      do                        return High

    --     do comma; symbol "*";     return High
    -- <|> do comma; pString "high"; return High
    -- <|> do comma; symbol "_";     return Low
    -- <|> do comma; pString "low";  return Low
    -- <|> do  


--------------------------
--- Policy expressions ---
--------------------------
pPolicy :: Parser Expression
pPolicy = do
    pol <- braces pols
    return $ EPol pol
   <|> do EVar <$> atom

pols :: Parser [Pol]
pols = sepBy1 pol comma

pol :: Parser Pol
pol = do
    t <- tag; colon
    Pol t <$> language


--------------------------
---     Relations      ---
--------------------------

operator :: Parser Op
operator = lexeme $ 
        do less;         return Less
    <|> do greater;      return Greater
    <|> do equal; equal; return Eq

relation :: Parser Relation
-- relation = 
--         try (do pString "eval";  symbol "("
--                 from <- atom;    comma
--                 to   <- atom;    comma
--                 pol  <- pPolicy; symbol")"
--                 return $ REval from to pol)
--     <|> try (do name <- atom
--                 pString "in"
--                 RIn name <$> atom)
--     <|> try (do group <- atom
--                 op <- operator
--                 RSize group op <$> integer)
--     <|> do pString "not "
--            RNot <$> relation

relation = 
        do  pString "not"
            RNot <$> relation
    <|> do  pString "eval";  symbol "("
            from <- atom;    comma
            to   <- atom;    comma
            pol  <- pPolicy; symbol")"
            return $ REval from to pol
    <|> try (do name <- atom
                pString "in"
                RIn name <$> atom)
    <|> try (do group <- atom
                op <- operator
                RSize group op <$> integer)
    <|> fail "Relation not properly formattet."

--------------------------


