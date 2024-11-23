{-# LANGUAGE InstanceSigs #-}

module TPL.Interface
    ( runComputePolicy
    , runComputeTrust
    , runQueryTrust
    , runCommands
    , prettyTrustPaths
    , prettyTypeTable
    , prettyTrustStore
    , getAllIdentities
    , module TPL.AST
    , TrustStore
    ) where

import           TPL.AST
import           TPL.API
import           TPL.Parser
import           TPL.StaticCheck
import           TPL.LatticeOps
import           Common.Reduce
import           Common.Pretty
import qualified Data.Map.Strict as M

-------------------------------------------------
-- | Functions for interfacing with the simulator
-------------------------------------------------

type Result a = Either String (TypeTable, TrustStore, a)

runComputePolicy :: Bool -> NetworkArg -> FilePath
                    -> [PolArg] -> Result SuperPolicy
runComputePolicy isMeet narg path pargs = do
    let op = if isMeet then runSuperPolicyMeets
                       else runSuperPolicyJoins
    network <- runNetworkParser path narg
    ppols <- parseMultiple runPrePoliciesParser "POLICY" pargs
    (ttab, tstore) <- convertResult $ runNetworkCheck network
    pols <- convertResult $ runPrePoliciesChecks ppols ttab
    pol <- convertResult (op pols ttab)
    return (ttab, tstore, reduceSuperPolicy pol)

runComputeTrust :: NetworkArg -> FilePath -> Identity
                    -> Identity -> Result SuperPolicy
runComputeTrust narg path i1 i2 = do
    network <- runNetworkParser path narg
    i1' <- runIdentityParser "IDENTITY1" i1
    i2' <- runIdentityParser "IDENTITY2" i2
    (ttab, tstore) <- convertResult $ runNetworkCheck network
    pol <- convertResult $ performComputation i1' i2' ttab tstore
    return (ttab, tstore, reduceSuperPolicy pol)

runQueryTrust :: ShowPathsFlag -> NetworkArg -> FilePath -> Identity
                -> Identity -> [PolArg] -> Result TrustPaths
runQueryTrust showpath narg path i1 i2 pargs = do
    network <- runNetworkParser path narg
    ppols <- parseMultiple runPrePoliciesParser "POLICY" pargs
    (ttab, tstore) <- convertResult $ runNetworkCheck network
    let stopAtFirst = not showpath
        ppol = foldl (<>) [] ppols
    pol <- convertResult (runPrePoliciesCheck ppol ttab)
    i1' <- runIdentityParser "IDENTITY1" i1
    i2' <- runIdentityParser "IDENTITY2" i2
    pathss <- convertResult $ performAuthorization stopAtFirst (i1', i2', reduceSuperPolicy pol) ttab tstore
    return (ttab, tstore, pathss)


---------------------------------
-- | Mainly for the web interface
---------------------------------

type CommandsArg = NetworkArg

data Output e = OutNetPolicy       Identity Identity SuperPolicy
              | OutComputedPolicy  Bool [SuperPolicy] SuperPolicy
              | OutTrustPaths      Identity Identity SuperPolicy TrustPaths
              | OutError           WebCommand e

runCommands :: (NetworkArg, FilePath) -> (CommandsArg, FilePath)
            -> Result [Output String]
runCommands (narg, npath) (carg, cpath) = do
    network <- runNetworkParser npath narg
    cmds <- runCommandsParser cpath carg
    env@(ttab, tstore) <- convertResult $ runNetworkCheck network
    let outs = map (runCommand env) cmds
    return (ttab, tstore, outs)
  where
    runCommand (ttab, _) c@(ComputePolicy isMeet ppols) =
        let op = if isMeet then runSuperPolicyMeets
                           else runSuperPolicyJoins
        in case runPrePoliciesChecks ppols ttab of
            Right pols -> case op pols ttab of
                Right pol -> OutComputedPolicy isMeet pols pol
                Left err -> OutError c $ pretty err
            Left err -> OutError c $ pretty err
    runCommand (ttab, tstore) c@(ComputeTrust i1 i2) =
        case performComputation i1 i2 ttab tstore of
            Right pol -> OutNetPolicy i1 i2 pol
            Left err -> OutError c $ pretty err
    runCommand (ttab, tstore) c@(QueryTrust i1 i2 ppols showpaths) =
        let stopAtFirst = not showpaths
            ppol = foldl (<>) [] ppols
        in case runPrePoliciesCheck ppol ttab of
            Right pol -> case performAuthorization stopAtFirst (i1, i2, reduceSuperPolicy pol) ttab tstore of
                Right pathss -> OutTrustPaths i1 i2 pol pathss
                Left err -> OutError c $ pretty err
            Left err -> OutError c $ pretty err


-------------------------------------------
-- | Handling parsing for multiple entities
-------------------------------------------

type ParserRunner a = FilePath -> CliArg -> Either String a

parseMultiple :: ParserRunner a -> String -> [CliArg] -> Either String [a]
parseMultiple parser metavar ls =
    convertResults $ zipWith (\i s -> parser ("<" ++ metavar ++ show i ++ ">") s)
                            ([1 .. ] :: [Int]) ls
  where convertResults = mapM convertResult


------------------------------
-- | Pretty-printing of output
------------------------------

instance Pretty e => Pretty (Output e) where
    pretty :: Output e -> String
    pretty (OutNetPolicy i1 i2 pol) = pretty (ComputeTrust i1 i2 :: WebCommand) <> "?" <\\> pretty pol
    pretty (OutComputedPolicy isMeet pols pol) = pretty (ComputePolicy isMeet pols) <> "?" <\\> pretty pol
    pretty (OutTrustPaths i1 i2 pol pathss) = pretty (QueryTrust i1 i2 [pol] True) <> "?" <\\> prettyTrustPaths pathss
    pretty (OutError c err) = pretty c <> "?" <\\> pretty err

prettyTrustPaths :: TrustPaths -> String
prettyTrustPaths pathss = do
    prettySepBy (line <> line) $ map ( \(p, paths) -> do
        "Sub-policy '" <> pretty p <> "':"
        <\> if null paths then "No paths found."
            else prettySepBy line
                    $ map (prettyPadSepBy  "->" . map pretty) paths
        ) pathss

prettyTypeTable :: TypeTable -> String
prettyTypeTable = pretty . specFromTTab

prettyTrustStore :: TrustStore -> String
prettyTrustStore tstore =
    let padding = maximum (map (length . pretty) $ getAllIdentities tstore)
    in prettySepBy line $ map (prettyRelation padding) $ M.toList tstore

prettyRelation :: Int -> (Identity, M.Map Identity SuperPolicy) -> String
prettyRelation padding (i, edges) = pad i padding <+> prettyEdges padding edges

prettyEdges :: Int -> M.Map Identity SuperPolicy -> String
prettyEdges padding = prettySepBy' (line <> fill padding) . map (prettyEdge padding) . M.toList

prettyEdge :: Int -> (Identity, SuperPolicy) -> String
prettyEdge padding (i, pol) = "=>" <+> pad i padding <+> colon <+> pretty pol

pad :: Pretty a => a -> Int -> String
pad i n = pretty i <> fill (n - length (pretty i))

fill :: Int -> String
fill n = replicate n ' '