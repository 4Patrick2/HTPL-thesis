{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE InstanceSigs      #-}

module TPL.Error
    ( TplErrors
    , Error(..)
    , module Common.Error
    ) where

import Common.AST
import Common.Error
import Common.Pretty

---------------
-- | TPL errors
---------------

type TplErrors = SomeErrors Error

data Error = UndefinedATag ATag
           | DuplicateATagInSpec ATag
           | DuplicateATagInPol  ATag
           | TypeMismatch  ATag ALang ALangType InferredType
           | TypesNonInferable [ATag]
    deriving (Eq, Show)


----------------------------------
-- | Pretty-printing of TPL errors
----------------------------------

instance Pretty Error where
    pretty :: Error -> String
    pretty (UndefinedATag atag) =
        "Undefined aspect tag" <> line
        <> "Aspect tag \"" <> pretty atag <> "\" is not part of the language specification"
    pretty (DuplicateATagInSpec atag) =
        "Duplicate aspect tags" <> line
        <> "Aspect tag \"" <> pretty atag <> "\" occurred twice in the language specification"
    pretty (DuplicateATagInPol atag) =
        "Duplicate aspect tags" <> line
        <> "Aspect tag \"" <> pretty atag <> "\" occurred twice in the policy"
    pretty (TypeMismatch atag alang expType actType) =
        "Type mismatch" <> line
        <> "Aspect language \""             <>  pretty alang <> "\"" <> line
        <> "with associated aspect tag \""  <>  pretty atag  <> "\"" <> line
        <> "was expected to have type"      <+> pretty expType       <> line
        <> "but was inferred to have type"  <+> pretty actType
    pretty (TypesNonInferable atags) =
        "The unified types of the following aspect tags could not be inferred:"
        <+> prettySepBy' comma atags