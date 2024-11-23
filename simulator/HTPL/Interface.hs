{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE InstanceSigs      #-}

module HTPL.Interface
    ( runComputeTrust
    , runAuthorizeTrust
    , runCommands
    , Output(..)
    , module HTPL.AST
    , module Common.Pretty
    ) where

import           HTPL.AST
import           HTPL.Eval
import           HTPL.SpecCheck
import           HTPL.Parser
import qualified TPL.API         as TPL
import           Common.Pretty

----------------------------------------
-- | Functions for interfacing with HTPL
----------------------------------------

type Result a   = Either String (TPL.TypeTable, TPL.TrustStore, a)
type NetworkArg = AuthArg

runComputeTrust :: NetworkArg -> FilePath -> Identity
                -> Identity -> Result SuperPolicy
runComputeTrust narg npath i1 i2 = do
    network <- runNetworkParser npath narg
    i1' <- runIdentityParser "IDENTITY1" i1
    i2' <- runIdentityParser "IDENTITY2" i2
    env@(_, ttab) <- convertResult $ runSpecCheck $ spec network
    (_, tstore) <- convertResult $ runLangConfigEval (lconfig network) env
    pol <- convertResult $ TPL.performComputation i1' i2' ttab tstore
    return (ttab, tstore, pol)

runAuthorizeTrust :: NetworkArg -> FilePath -> AuthArg -> Result [UniTables]
runAuthorizeTrust narg npath aarg = do
    network <- runNetworkParser npath narg
    pauth <- runPreAuthorizationParser "AUTHORIZATION" aarg
    env@(_, ttab) <- convertResult $ runSpecCheck $ spec network
    (symtab, tstore) <- convertResult $ runLangConfigEval (lconfig network) env
    utabss <- convertResult $ runAuthorizationEval pauth env tstore symtab
    return (ttab, tstore, utabss)


---------------------------------
-- | Mainly for thw web interface
---------------------------------

type CommandsArg = AuthArg

data Output e = OutPolicy        Identity Identity SuperPolicy
              | OutUniTablesList PreAuthorization  [UniTables]
              | OutError         WebCommand e

runCommands :: (NetworkArg, FilePath) -> (CommandsArg, FilePath)
                -> Result [Output String]
runCommands (narg, npath) (carg, cpath) = do
    network <- runNetworkParser npath narg
    cmds <- runCommandsParser cpath carg
    env@(_, ttab) <- convertResult $ runSpecCheck $ spec network
    (symtab, tstore) <- convertResult $ runLangConfigEval (lconfig network) env
    let outs = map (runCommand tstore env symtab) cmds
    return (ttab, tstore, outs)
  where
    runCommand tstore env symtab c@(AuthorizeTrust pauth) = do
        case runAuthorizationEval pauth env tstore symtab of
            Right utabss -> OutUniTablesList pauth utabss
            Left err -> OutError c $ pretty err
    runCommand tstore (_, ttab) _ c@(ComputeTrust i1 i2) =
        case TPL.performComputation i1 i2 ttab tstore of
            Right pol -> OutPolicy i1 i2 pol
            Left err -> OutError c $ pretty err


------------------------------
-- | Pretty-printing of output
------------------------------

instance Pretty e => Pretty (Output e) where
    pretty :: Output e -> String
    pretty (OutPolicy i1 i2 pol) =
        pretty (ComputeTrust i1 i2 :: Command PreAuthorization) <> "?"
        <\\> pretty pol
    pretty (OutUniTablesList pauth utabss) =
        pretty pauth <> "?"
        <\\> if hasSolution utabss
             then "Trust paths found!"
                <> if hasBindings utabss
                   then line <> line <> prettyUniTablesList utabss
                   else empty
             else "No trust paths found."
    pretty (OutError c err) = pretty c <> "?" <\\> pretty err