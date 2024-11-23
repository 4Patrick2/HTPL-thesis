{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE InstanceSigs       #-}

module TPL.AST
    ( module TPL.AST
    , module Common.AST
    ) where

import           Common.AST
import           Common.Pretty
import           Common.IOStuff     ( Input )
import qualified Data.Text          as T

--------------------------------------
-- | A TPL is a language specification
--------------------------------------

data Network = Network {spec :: Spec, dconfig :: DelConfig}
    deriving (Eq, Show)

newtype Spec = Spec {assocs :: [Assoc]}
    deriving (Eq, Show)

data Assoc = Assoc {atag :: ATag, typing :: Typing}
    deriving (Eq, Show)

data Typing = Typing ALangType (Maybe ALang)
    deriving (Eq, Show)

newtype DelConfig = DelConfig {delegs :: [Delegation]}
    deriving (Eq, Show)

type Delegation = (Identity, Identity, PrePolicies)

type Authorization = (Identity, Identity, SuperPolicy)


------------------------------------------------
-- | Commands for interfacing with the simulator
------------------------------------------------

type CliArg = T.Text

type NetworkArg    = CliArg
type ALangArg      = CliArg
type PolArg        = CliArg
type ShowPathsFlag = Bool

data CliCommand = CliCommand Input CliSubCommand
    deriving (Eq, Show)

type CliSubCommand = Command PolArg

data Command p
    = ComputePolicy Bool [p]
    | ComputeTrust  Identity Identity
    | QueryTrust    Identity Identity [p] ShowPathsFlag
    deriving (Eq, Show)

type WebCommand = Command PrePolicies


---------------------------------
-- | Pretty printing of AST nodes
---------------------------------

instance Pretty Spec where
    pretty :: Spec -> String
    pretty (Spec decls) =
        prettySepBy comma decls

instance Pretty Assoc where
    pretty :: Assoc -> String
    pretty (Assoc atag typing) =
        pretty atag <> colon <> pretty typing

instance Pretty Typing where
    pretty :: Typing -> String
    pretty (Typing tp ml) = case ml of
        Nothing -> pretty tp
        Just l -> "with_default" <>
            parens (pretty tp <> comma <> pretty l)

instance Pretty p => Pretty (Command p) where
    pretty :: Command p -> String
    pretty (ComputePolicy isMeet ps) = (if isMeet then "meet" else "join") <> parens (prettySepBy' comma ps)
    pretty (ComputeTrust i1 i2) = pretty i1 <+> "=>" <+> pretty i2
    pretty (QueryTrust i1 i2 ps _) = pretty i1 <+> "=>" <+> pretty i2 <+> colon <+> prettySepBy semi ps