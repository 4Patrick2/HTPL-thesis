{-# LANGUAGE OverloadedStrings    #-}

module Common.Lexer
    ( spaceConsumer
    , symbol
    , parens, braces, brackets, angles
    , semi, comma, colon, colonn, dot
    , equal, arrow, aarrow, mid, question
    , minus, plus, pluss, times, divide
    , asterisk, underscore
    , sep
    , flag
    , number
    , reserved
    , atomIdent
    , atagIdent
    , name
    , ident
    , identity
    , identityAt
    ) where

import           Common.Parser.Type
import           Common.AST
import           Text.Megaparsec
import           Text.Megaparsec.Char
import           Control.Monad.Reader
import qualified Data.Text                   as T
import qualified Text.Megaparsec.Char.Lexer  as L

-----------------------------------
-- | Lexing the various token types
-----------------------------------

spaceConsumer :: Parser ()
spaceConsumer = L.space space1 (L.skipLineComment "//") (L.skipBlockComment "/*" "*/")

lexeme :: Parser a -> Parser a
lexeme = L.lexeme spaceConsumer

symbol :: T.Text -> Parser ()
symbol = void . L.symbol spaceConsumer

parens, braces, brackets, angles :: Parser a -> Parser a
parens   = between (symbol "(") (symbol ")")
braces   = between (symbol "{") (symbol "}")
brackets = between (symbol "[") (symbol "]")
angles   = between (symbol "<") (symbol ">")

semi, comma, colon, colonn, dot     :: Parser ()
equal, arrow, aarrow, mid, question :: Parser ()
semi  = symbol ";" ; comma    = symbol ","
colon = symbol ":" ; colonn   = symbol "::"
dot   = symbol "." ; equal    = symbol "="
arrow = symbol "->"; aarrow   = symbol "=>"
mid   = symbol "|" ; question = symbol "?"

minus, plus, pluss, times, divide :: Parser ()
minus = symbol "-"
plus = symbol "+"
pluss = symbol "++"
times = symbol "*"
divide = symbol "/"

asterisk, underscore :: Parser ()
asterisk   = times
underscore = symbol "_"

sep :: Parser ()
sep = question

flag :: T.Text -> Parser Bool
flag w = isJust <$> optional (reserved w)
  where
    isJust :: Maybe a -> Bool
    isJust (Just _) = True
    isJust _ = False

number :: Parser Int
number = lexeme L.decimal

reserved :: T.Text -> Parser ()
reserved w = lexeme $ string w *> notFollowedBy nameChar

atomIdent :: Parser Ident
atomIdent = ident 1 "Atom"

atagIdent :: Parser Ident
atagIdent = ident 2 "Aspect tag"

ident :: Int -> String -> Parser Ident
ident minLength constr = do
    n <- name
    keywords <- ask
    when (isKeyword n keywords) $
        fail $ "Keyword " ++ T.unpack n ++ " used as identifier"
    when (T.length n < minLength) $
        fail $ constr
            ++ " has be at least "
            ++ show minLength
            ++ " characters long"
    return n
  where isKeyword s keywords = s `elem` keywords

name :: Parser T.Text
name = lexeme name'

name' :: Parser T.Text
name' = T.pack <$> (
    (:) <$> letterChar
        <*> many nameChar
        <?> "identifier"
    )

nameChar :: Parser Char
nameChar = alphaNumChar <|> char '-' <|> char '_'

identityAt :: Parser T.Text
identityAt = lexeme (char '@' *> name')

identity :: Parser Identity
identity = ident 1 "Identity"