{-# LANGUAGE OverloadedStrings #-}

module Parser 
    ( -- runProgramParser
    runNetworkParser
    , runImportParser
    -- , Parser
    ) where

import AST
import Parser.ParserImpl
import Parser.Lexer
import qualified Data.Text as T
import qualified Text.Megaparsec  as P
import Control.Monad.Reader

-- -- Parser monad
-- type Parser = ReaderT [T.Text] (Parsec Void T.Text)


runParser :: Parser a -> String -> T.Text -> [T.Text] -> Either String a 
runParser p f c keywords =
    case P.runParser ( runReaderT 
                        (spaceConsumer *> p <* P.eof) keywords) f c of
        Left err -> Left $ P.errorBundlePretty err
        Right r -> Right r


-- runParser :: Parser a -> String -> T.Text -> [T.Text] -> Either String a 
-- runParser p f s keywords = 
--     case P.parse p f s of 
--         Left err -> Left (P.errorBundlePretty err)
--         Right r -> Right r 

-- runProgramParser :: String -> T.Text -> Either String Program
-- runProgramParser f s = runParser pProgram f s reservedIdentifiers

runNetworkParser :: String -> T.Text -> Either String Network
runNetworkParser f s = runParser pNetwork f s reservedIdentifiers

runImportParser :: String -> T.Text -> Either String Relation
runImportParser f s = runParser relation f s reservedIdentifiers



reservedIdentifiers :: [T.Text]
reservedIdentifiers = ["lang", "policy", "pred", "if", "when", "for", "with", "otherwise", "then", "in"]
