{-# LANGUAGE OverloadedStrings    #-}

module HTPL.Parser.Impl
    ( parseNetwork
    , parsePreAuthorization
    , parseCommands
    ) where

import HTPL.AST
import Common.Parser
import Common.Lexer
import Text.Megaparsec
import Control.Monad.Combinators.Expr
import Common.Parser.Impl (parseIdentity)

----------------
-- | Sub-parsers
----------------

parseNetwork :: Parser Network
parseNetwork = Network <$> parseSpec <*> parseLangConfig

-- | Variables
parseATagVar :: Parser Ident
parseATagVar = angles atagIdent

parseGVar :: Parser Ident
parseGVar = ident 1 "Group identifier"

parseSPVar :: Parser Ident
parseSPVar = ident 1 "Superpolicy identifier"

parseNVar :: Parser Ident
parseNVar = ident 1 "Variable"


-- | Top-level specification

parseSpec :: Parser Spec
parseSpec = do
    isVersioned <- flag "versioned"
    reserved "language"
    is <- sepBy parseInclusion comma
    ds <- braces (some parseDeclaration) <|> return []
    return $ Spec isVersioned is ds

parseInclusion :: Parser Inclusion
parseInclusion = do
    t <- parseATagVar
    l <- optional (parens parseDefault <|> parseDefault)
    return $ Inclusion t l

parseDefault :: Parser ALang
parseDefault = reserved "with" *> parseALang

parseDeclaration :: Parser Declaration
parseDeclaration = Declaration <$> parseATagVar
                               <*> optional (colonn *> parseALangType)
                               <*> (equal *> parseStructure <|> return (Structure [LTopS]))

parseStructure :: Parser Structure
parseStructure = do
    terms <- sepBy1 parseStrucTerm mid
    return $ Structure terms

parseStrucTerm :: Parser StrucTerm
parseStrucTerm =
        (AtomS <$> atomIdent)
    <|> braces (TDNSS <$> parseTree parseStrucTerm LBotS)
    <|> (StrucVar <$> parseATagVar)
    <|> parseTop LTopS <|> parseBot LBotS

-- | Language configuration
parseLangConfig :: Parser LangConfig
parseLangConfig = LangConfig <$> many parseStatement

parseStatement :: Parser Statement
parseStatement = do
    isLocal <- flag "local"
    stmt <- parseRawStatement
    return $ stmt isLocal

type RawStatement = (LocalFlag -> Statement)

parseRawStatement :: Parser RawStatement
parseRawStatement = parseGBinding
                <|> parseNBinding
                <|> parseSPBinding
                <|> parseDelegation
                <?> "statement"

parseGBinding :: Parser RawStatement
parseGBinding = do
    reserved "group"
    i <- parseGVar
    equal
    GBinding i <$> parseGroupExpr

parseNBinding :: Parser RawStatement
parseNBinding = do
    reserved "word"
    i <- parseNVar
    equal
    NBinding i <$> parseNExpr

parseSPBinding :: Parser RawStatement
parseSPBinding = do
    reserved "policy"
    i <- parseSPVar
    equal
    SPBinding i <$> parseSPExpr

parseDelegation :: Parser RawStatement
parseDelegation = do
    isRestricted <- flag "restricted"
    reserved "trust"
    drel <- parseDRelation
    equal
    Delegation isRestricted drel <$> parseSPExpr

-- | Group expressions
parseGroupExpr :: Parser GroupExpr
parseGroupExpr = makeExprParser parseGroupExprTerm operatorTable
  where
    parseGroupExprTerm = parens parseGroupExpr
            <|> GConst <$> brackets parseList
            <|> GName <$> parseGVar

    parseList = sepBy identity comma

    operatorTable = [ [ InfixL (GAppend <$ symbol "++") ] ]

-- | Arithmetic expressions
parseNExpr :: Parser NExpr
parseNExpr = makeExprParser parseNExprTerm operatorTable
  where
    parseNExprTerm :: Parser NExpr
    parseNExprTerm = parens parseNExpr
                 <|> (NLen <$> (reserved "size" *> parens parseGroupExpr))
                 <|> (NVar <$> parseNVar)
                 <|> (NConst <$> number)

    operatorTable :: [[Operator Parser NExpr]]
    operatorTable = [ [ InfixL (NBinOp Times <$ times), InfixL (NBinOp Divide <$ divide) ]
                    , [ InfixL (NBinOp Plus  <$ plus),  InfixL (NBinOp Minus  <$ minus) ]
                    ]

-- | Policy expressions
parseSPExpr :: Parser SPExpr
parseSPExpr = makeExprParser parseSPExprTerm operatorTable
  where
    parseSPExprTerm :: Parser SPExpr
    parseSPExprTerm = parens parseSPExpr
         <|> try ((SPVar <$> parseSPVar) <* notFollowedBy colon)
         <|> (SPConst <$> parsePrePolicy)

    operatorTable :: [[Operator Parser SPExpr]]
    operatorTable =
      [ [ InfixL (SPAppend <$ symbol ";") ]
      ]

-- | Relational expressions
parseRelation :: String -> Parser a -> Parser (Relation a)
parseRelation kind p = do
    l <- p
    arrow
    try (More l <$> parseRelation kind p)
      <|> One l <$> p
      <?> kind ++ " relation"

parseDRelation :: Parser DRelation
parseDRelation = parseRelation "delegation" parseDEntities

parseDEntities :: Parser DEntities
parseDEntities = DEntities <$> sepBy1 parseDEntity comma

parseDEntity :: Parser DEntity
parseDEntity = parseDAll <|> parseDSingle
  where
    parseDAll :: Parser DEntity
    parseDAll = do
        reserved "all"
        DAll <$> parens parseGroupExpr

    parseDSingle :: Parser DEntity
    parseDSingle = DSingle <$> identity

parseARelation :: Parser ARelation
parseARelation = parseRelation "authorization" parseAEntities

parseAEntities :: Parser AEntities
parseAEntities = makeExprParser parseAEntitiesTerm operatorTable
  where
    parseAEntitiesTerm :: Parser AEntities
    parseAEntitiesTerm = parens parseAEntities
         <|> reserved "choose" *> parens (AChoose <$> (parseNExpr <* comma) <*> parseGroupExpr)
         <|> AAll <$> (reserved "all" *> parens parseGroupExpr)
         <|> AAny <$> (reserved "any" *> parens parseGroupExpr)
         <|> ASingle <$> identity
         <|> AVar <$> identityAt

    operatorTable :: [[Operator Parser AEntities]]
    operatorTable =
      [ [ InfixL (AAnd <$ symbol ",") ]
      , [ InfixL (AOr <$ symbol ";") ]
      ]

parsePreAuthorization :: Parser PreAuthorization
parsePreAuthorization = makeExprParser parsePreAuthorizationTerm operatorTable
  where
    parsePreAuthorizationTerm :: Parser PreAuthorization
    parsePreAuthorizationTerm =
         try ( do {
              arel <- parseARelation;
              colon;
              Authorization arel <$> parseSPExpr })
         <|> parens parsePreAuthorization

    operatorTable :: [[Operator Parser PreAuthorization]]
    operatorTable =
      [ [ InfixL (AuthCon <$ symbol "&&") ]
      , [ InfixL (AuthDis <$ symbol "||") ]
      ]

parseCommands :: Parser [WebCommand]
parseCommands = sepEndBy parseCommand sep

parseCommand :: Parser WebCommand
parseCommand = try parseAuthorize <|> parseCompute

parseAuthorize :: Parser WebCommand
parseAuthorize = AuthorizeTrust <$> parsePreAuthorization

parseCompute :: Parser WebCommand
parseCompute = do
  i <- parseIdentity
  aarrow
  ComputeTrust i <$> parseIdentity