{-# LANGUAGE OverloadedStrings #-}

module HTPL.Parser
    ( runNetworkParser
    , runPreAuthorizationParser
    , runCommandsParser
    , runIdentityParser
    ) where

import           HTPL.AST
import           HTPL.Parser.Impl
import qualified Common.Parser      as Common
import qualified Data.Text          as T

----------------------
-- | Top-level parsers
----------------------

runNetworkParser :: String -> T.Text -> Either String Network
runNetworkParser f s = Common.runSomeParser parseNetwork f s keywords

runPreAuthorizationParser :: String -> T.Text -> Either String PreAuthorization
runPreAuthorizationParser f s = Common.runSomeParser parsePreAuthorization f s keywords

runCommandsParser :: String -> T.Text -> Either String [WebCommand]
runCommandsParser f s = Common.runSomeParser parseCommands f s keywords

runIdentityParser :: String -> T.Text -> Either String Identity
runIdentityParser f s = Common.runIdentityParser f s keywords

keywords :: [T.Text]
keywords = [
      "versioned"
    , "language"
    , "with"
    , "local"
    , "group"
    , "word"
    , "policy"
    , "trust"
    , "restricted"
    , "size"
    , "all"
    , "choose"
    , "any"
    , "pin"
    , "ver"
    ]
