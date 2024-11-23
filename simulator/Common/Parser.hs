{-# LANGUAGE OverloadedStrings #-}

module Common.Parser
    ( runSomeParser
    , runPrePoliciesParser
    , runALangParser
    , runIdentityParser
    , parsePrePolicies
    , parsePrePolicy
    , parseALang
    , parseALangType
    , parseTree
    , parseBot
    , parseTop
    , Parser
    ) where

import Common.AST
import Common.Parser.Impl
import Control.Monad.Reader
import qualified Data.Text        as T
import qualified Text.Megaparsec  as P

---------------------------
-- | Main parsing functions
---------------------------

runPrePoliciesParser :: String -> T.Text -> [T.Text]
                        -> Either String PrePolicies
runPrePoliciesParser = runSomeParser parsePrePolicies

runIdentityParser :: String -> T.Text -> [T.Text]
                     -> Either String Identity
runIdentityParser = runSomeParser parseIdentity

runALangParser :: String -> T.Text -> [T.Text]
                  -> Either String ALang
runALangParser = runSomeParser parseALang

runSomeParser :: Parser a -> String -> T.Text -> [T.Text]
                 -> Either String a
runSomeParser p f c keywords =
    case P.runParser ( runReaderT
                        (spaceConsumer *> p <* P.eof)
                        $ commonKeywords ++ keywords ) f c of
       Left err -> Left $ P.errorBundlePretty err
       Right r  -> Right r

commonKeywords :: [T.Text]
commonKeywords =
    [ "atomic"
    , "tdns"
    ]