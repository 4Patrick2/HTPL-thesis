{-# LANGUAGE OverloadedStrings #-}

module Parser 
    ( runNetworkParser
    , runLOParser
    , runImportParser
    ) where

import AST
import Parser.ParserImpl
import Parser.Lexer
import qualified Data.Text as T
import qualified Text.Megaparsec  as P

-- Executes a parser ensureing the eof is reached. 
runParser :: Parser a -> String -> T.Text -> Either String a 
runParser p f s = 
    case P.parse (spaceConsumer *> p <* P.eof) f s of 
        Left err -> Left $ P.errorBundlePretty err
        Right r -> Right r 

-- Runs the network parser. Returns a network or error message.
runNetworkParser :: String -> T.Text -> Either String Network
runNetworkParser f s = runParser pNetwork f s

-- Parses import statements. Does not reach eof.
runImportParser :: String -> T.Text -> Either String [Import]
runImportParser f s = 
    case P.parse pImports f s of 
        Left err -> Left $ P.errorBundlePretty err
        Right r -> Right r 

-- Parser function used for testing individual parsers.
runLOParser :: String -> T.Text -> Either String LanguageOptions
runLOParser f s = runParser pLangDefs f s 