{-# LANGUAGE OverloadedStrings #-}

module Parser 
    ( runNetworkParser
    , runTestParser
    ) where

import AST
import Parser.ParserImpl
import Parser.Lexer
import qualified Data.Text as T
import qualified Text.Megaparsec  as P


runParser :: Parser a -> String -> T.Text -> Either String a 
runParser p f s = 
    case P.parse (spaceConsumer *> p <* P.eof) f s of 
        Left err -> Left $ P.errorBundlePretty err
        Right r -> Right r 

runNetworkParser :: String -> T.Text -> Either String Network
runNetworkParser f s = runParser pNetwork f s

runTestParser :: String -> T.Text -> Either String LanguageOptions
runTestParser f s = runParser pLangDef f s 
