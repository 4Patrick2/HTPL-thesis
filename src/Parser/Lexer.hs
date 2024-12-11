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
import qualified Data.Map.Strict        as M
import Data.List (notElem)

-- Parser monad
type Parser = Parsec Void T.Text

---------------
---- Lexing ---
---------------

spaceConsumer :: Parser ()
spaceConsumer = L.space space1 (L.skipLineComment "#") empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme spaceConsumer

symbol :: T.Text -> Parser T.Text
symbol = L.symbol spaceConsumer

integer :: Parser Int
integer = lexeme $ do
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

----------------
--- Literals ---
----------------

-- Reserved keywords of HTPL. 
reserved = ["if", "for", "else", "otherwise", "then", "when", "do", "policy", "where", "group", "pred", "trust", "eval", "lang"]

-- Parsing Aspect tags.
-- Must start with capital letter. Can not be a keyword.
tag :: Parser ATag
tag = lexeme $ do
    t <- tag'
    T.pack <$> checkKeyword t

tag' :: Parser String
tag' = (:) <$> upperChar <*> many tagTail <|> fail "Aspect tag not properly formattet."

tagTail :: Parser Char
tagTail = alphaNumChar <|> char '_'

-- Variable name parsing.
-- Begins with a capital letter which can be followed by any letter or number.
variable :: Parser VName
variable = lexeme $ T.pack <$> variable'

variable' :: Parser String
variable' = (:) <$> upperChar <*> many alphaNumChar <|> fail "Variable name not properly formed."

-- User name parsing.
-- Begins with a lower case letter followed by letters and numbers. 
user :: Parser User
user = lexeme $ do
    u <- user'
    T.pack <$> checkKeyword u

user' :: Parser String
user' = (:) <$> lowerChar <*> many alphaNumChar <|> fail "User name must begin with lower case character and can not contain special symbols."

-- The filenames can contain any letter and number and must end using the suffix “.lan”.
filename :: Parser FName
-- filename = lexeme $ T.pack <$> filename' <|> fail "Filename not properly formattet."
filename = lexeme $ do
    filepath <- filename'
    return $ T.pack $ filepath++".lan"
     <|> fail "Filename not properly formattet."

filename' :: Parser String
filename' = lexeme $ someTill (printChar <|> char '/') (string ".lan") <|> fail "Filename not properly formattet."

checkKeyword :: String -> Parser String
checkKeyword word = do
    if word `notElem` reserved then return word
    else fail "Atom can not be a reserved keyword!"

-- Language expressions can begin with any alpha numeral. 
-- In HTPL language expressions are limited to the TDNS (Atomic) type.
languageAtom :: Parser LanguageAtom
languageAtom = lexeme $ do
    atom <- languageAtom'
    T.pack <$> checkKeyword atom

languageAtom' :: Parser String
languageAtom' = (:) <$> letterChar <*> many atomTail <|> fail "Language expression not properly formed."

atomTail :: Parser Char
atomTail = alphaNumChar <|> char '_' <|> char '-' <|> char '@' <|> char '$'

userOrVariable :: Parser T.Text
userOrVariable = try variable <|> try user <|> fail "Atom ill-formed."

---------------------------
--- Langauge expression ---
---------------------------

-- Parse langauge expression.
-- Can only parse type TDNS(Atomic)
language :: Parser ALang
language = try ( do
    f <- aLang
    s <- brackets aLang
    _ <- dot
    t <- aLang
    return $ TDNS ( Node f (Leaf s) (Leaf t) ))
    <|> try (do
    f <- aLang
    s <- brackets aLang
    return $ TDNS $ Node f (Leaf s) (Leaf LTop) )
    <|> try (do
    f <- aLang
    _ <- dot
    t <- aLang
    return $ TDNS $ Node f (Leaf LTop) (Leaf t) )
    <|> try (do
    f <- aLang
    return $ TDNS $ Node f (Leaf LTop) (Leaf LTop) )
    <|>
    fail "Language expression not well formed."

aLang :: Parser ALang
aLang = do --lexeme $ do
    symbol "*"; return LTop
    <|> do
    symbol "_"; return LBot
    <|> do
    Atom <$> languageAtom

------------------
--- Predicates ---
------------------
preds :: Parser [Pred]
preds = sepBy1 predicate semicolon

predicate :: Parser Pred
predicate = do
    from <- userOrVariable; comma
    to   <- userOrVariable; colon
    Pred from to <$> pPolicy

--------------------------
--- Policy expressions ---
--------------------------

pPolicy :: Parser Expression
pPolicy = do
    pol <- braces policy_
    return $ EPol pol
   <|> do EVar <$> variable

pol :: Parser (ATag, ALang)
pol = do
    t <- tag; colon
    a <- language
    return (t, a)

policy_ :: Parser Policy
policy_ = do
    p <- M.fromList <$> pols
    return $ Policy p

pols :: Parser [(ATag, ALang)]
pols = sepBy1 pol comma

--------------------------
---     Relations      ---
--------------------------

operator :: Parser Op
operator = lexeme $
        do less;         return Less
    <|> do greater;      return Greater
    <|> do equal; equal; return Eq

relation :: Parser Relation
relation =
        do  pString "not"
            RNot <$> relation
    <|> do  pString "eval";     symbol "("
            from <- user;       comma
            to   <- user;       equal
            pol  <- pPolicy;    symbol ")"
            return $ REval from to pol
    <|> try (do name <- user
                pString "in"
                RIn name <$> variable)
    <|> try (do group <- variable
                op <- operator
                RSize group op <$> integer)
    <|> fail "Relation not properly formattet."


