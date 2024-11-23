{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}

module TPL.Parser.Impl
    ( parseNetwork
    , parseCommands
    ) where

import TPL.AST
import Common.Parser
import Common.Lexer
import Text.Megaparsec

----------------
-- | Sub-parsers
----------------

parseNetwork :: Parser Network
parseNetwork = Network <$> parseSpec <*> parseDelConfig

parseDelConfig :: Parser DelConfig
parseDelConfig = DelConfig <$> many parseDelegation

parseDelegation :: Parser Delegation
parseDelegation = do
    i1 <- identity
    arrow
    i2 <- identity
    colon
    (i1, i2,) <$> parsePrePolicies

parseSpec :: Parser Spec
parseSpec = Spec <$> sepBy1 parseAssoc comma

parseAssoc :: Parser Assoc
parseAssoc = do
    t <- atagIdent
    colon
    Assoc t <$> parseTyping

parseTyping :: Parser Typing
parseTyping = parseSimple <|> parseWithDef

parseSimple :: Parser Typing
parseSimple = Typing <$> parseALangType <*> return Nothing

parseWithDef :: Parser Typing
parseWithDef = reserved "with_default" *> parens (do
    tp <- parseALangType
    comma
    Typing tp . Just <$> parseALang)

parseCommands :: Parser [WebCommand]
parseCommands = sepEndBy parseCommand sep

parseCommand :: Parser WebCommand
parseCommand = try parseQueryTrust <|> try parseComputeTrust <|> parseComputePolicy <?> "command"

parseComputePolicy :: Parser WebCommand
parseComputePolicy = do
    isMeet <- (reserved "meet" >> return True) <|> (reserved "join" >> return False)
    ComputePolicy isMeet <$> parens (sepBy1 parsePrePolicies comma)

parseComputeTrust :: Parser WebCommand
parseComputeTrust = do
    i <- identity
    aarrow
    ComputeTrust i <$> identity

parseQueryTrust :: Parser WebCommand
parseQueryTrust = do
    i1 <- identity
    arrow
    i2 <- identity
    colon
    ppol <- parsePrePolicies
    return $ QueryTrust i1 i2 [ppol] True