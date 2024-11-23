{-# LANGUAGE OverloadedStrings #-}

module Common.Parser.Impl
    ( Parser
    , parsePrePolicies
    , parsePrePolicy
    , parseALang
    , parseTree
    , parseBot
    , parseTop
    , parseType
    , parseALangType
    , parseIdentity
    , spaceConsumer
    ) where

import Common.AST
import Common.Lexer
import Common.Parser.Type
import Text.Megaparsec     hiding ( Pos )
import Data.Maybe          ( fromMaybe )
import Data.Functor        ( ($>) )

----------------
-- | Sub-parsers
----------------

parseIdentity :: Parser Identity
parseIdentity = identity

parsePrePolicies :: Parser PrePolicies
parsePrePolicies = sepBy1 parsePrePolicy semi <?> "superpolicy"

parsePrePolicy :: Parser PrePolicy
parsePrePolicy = parseBot (Left False)
               <|> parseTop (Left True)
               <|> (Right <$> sepBy1 parseAspect comma)
               <?> "policy"

parseAspect :: Parser (ATag, ALang)
parseAspect = (,) <$> atagIdent <*> (colon >> parseALang) <?> "aspect"

parseALang :: Parser ALang
parseALang = parseTdns
         <|> parseAtom
         <|> parseTop LTop
         <|> parseBot LBot
         <?> "aspect language"

parseAtom :: Parser ALang
parseAtom = Atom <$> atomIdent

parseTop, parseBot :: a -> Parser a
parseTop sym = asterisk >> return sym
parseBot sym = underscore >> return sym

parseTdns :: Parser ALang
parseTdns = braces $ TDNS <$> parseTree parseALang LTop

parseTree :: Parser a -> a -> Parser (Tree a)
parseTree p deflt = do
    d <- p
    b <- optional $ brackets (parseTree p deflt)
    r <- optional (dot >> parseTree p deflt)
    case (b,r) of
        (Nothing, Nothing) -> return $ Leaf d
        (Just b', Just r') -> return $ Node d b' r'
        _ -> return $ Node d
                     (fromMaybe (Leaf deflt) b)
                     (fromMaybe (Leaf deflt) r)

parseType :: Parser a -> Parser (Type a)
parseType p = LeafT <$> p
      <|> TDNST <$> (reserved "tdns" >> parens (parseType p)) <?> "type"

parseALangType :: Parser ALangType
parseALangType = parseType parseAtomic

parseAtomic :: Parser ALangTypeTerm
parseAtomic = reserved "atomic" $> Atomic