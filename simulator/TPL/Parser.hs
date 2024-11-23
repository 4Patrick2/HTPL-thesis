{-# LANGUAGE OverloadedStrings #-}

module TPL.Parser
    ( runNetworkParser
    , runPrePoliciesParser
    , runALangParser
    , runIdentityParser
    , runCommandsParser
    ) where

import           TPL.AST
import           TPL.Parser.Impl
import qualified Common.Parser      as Common
import qualified Data.Text          as T

----------------------
-- | Top-level parsers
----------------------

runNetworkParser :: String -> T.Text -> Either String Network
runNetworkParser f s = Common.runSomeParser parseNetwork f s keywords

runCommandsParser :: String -> T.Text -> Either String [WebCommand]
runCommandsParser f s = Common.runSomeParser parseCommands f s keywords

runPrePoliciesParser :: String -> T.Text -> Either String PrePolicies
runPrePoliciesParser f s = Common.runPrePoliciesParser f s keywords

runIdentityParser :: String -> T.Text -> Either String Identity
runIdentityParser f s = Common.runIdentityParser f s keywords

runALangParser :: String -> T.Text -> Either String ALang
runALangParser f s = Common.runALangParser f s keywords

keywords :: [T.Text]
keywords = ["with_default"]