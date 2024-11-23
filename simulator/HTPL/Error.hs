{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE InstanceSigs      #-}

module HTPL.Error
    ( StaticErrors
    , RuntimeErrors
    , StaticError(..)
    , RuntimeError(..)
    , module Common.Error
    ) where

import           HTPL.AST
import           Common.Error
import           Common.Pretty
import qualified TPL.Error as TPL

----------------
-- | HTPL errors
----------------

type StaticErrors = SomeErrors StaticError
type RuntimeErrors = SomeErrors RuntimeError

data RuntimeError
    = NoGroupWithName Ident
    | NoNumWithName Ident
    | NoPolWithName Ident
    | IdentityDefinedAsGroup Identity
    | DivisionByZero NExpr
    | ALangNotAdheresTo ALang Structure
    | DefaultNotFollowingStructure ATag ALang Structure
    | VariableAlreadyBound Ident
    | TplError TPL.Error
    deriving (Eq, Show)

data StaticError
    = DuplicateATagInSpec ATag
    | DuplicateATagInInclusions ATag
    | UndefinedATag ATag
    | RefCycleDetected [ATag]
    | InconsistentType ATag
    | NonDeducableType ATag
    | NoClue ATag
    deriving (Eq, Show)


-----------------------------------
-- | Pretty-printing of HTPL errors
-----------------------------------

instance Pretty RuntimeError where
    pretty :: RuntimeError -> String
    pretty (NoGroupWithName i) =
        "No group with name \"" <> pretty i <> "\" has been defined"
    pretty (NoNumWithName i) =
        "No number with name \"" <> pretty i <> "\" has been defined"
    pretty (NoPolWithName i) =
        "No policy with name \"" <> pretty i <> "\" has been defined"
    pretty (IdentityDefinedAsGroup i) =
        "Identity \"" <> pretty i <> "\" has already been defined as a group"
    pretty (DivisionByZero nexpr) =
        "Division by zero in expression \"" <> pretty nexpr <> "\""
    pretty (ALangNotAdheresTo l struc) = "Aspect language \"" <> pretty l <> "\" does not adhere to its defined structure:" <> line
        <> "    " <> pretty struc
    pretty (DefaultNotFollowingStructure atag alang struc) =
        "Default aspect language \"" <>  pretty alang <> "\""
        <+> "for aspect tag \"<"  <> pretty atag  <> ">\""
        <+> "does not adhere to the defined structure"
        <+> "\"" <> pretty struc <> "\""
    pretty (VariableAlreadyBound x) =
        "Variable \"" <> pretty x <> "\" is defined more than once"
    pretty (TplError err) = pretty err

instance Pretty StaticError where
    pretty :: StaticError -> String
    pretty (InconsistentType atag) =
        "The type of \"<" <> pretty atag <> ">\" is inconsistent throughout the specification"
    pretty (NonDeducableType atag) =
        "The type of \"<" <> pretty atag <> ">\" is ambiguous throughout the specification"
    pretty (NoClue atag) =
        "The type of aspect tag \"<" <> pretty atag <> ">\" cannot be inferred"
    pretty (RefCycleDetected atags) =
        "A referential cycle was detected in the set of declarations:" <> line
        <> "    " <> pretty (prettyPadSepBy "~>" atags)
    pretty (UndefinedATag atag) =
        "Aspect tag \"<" <> pretty atag <> ">\" is not part of the language specification"
    pretty (DuplicateATagInSpec atag) =
        "Aspect tag \"<" <> pretty atag <> ">\" was declared more than once in the language specification"
    pretty (DuplicateATagInInclusions atag) =
        "Aspect tag \"<" <> pretty atag <> ">\" occurred more than once in inclusion list"