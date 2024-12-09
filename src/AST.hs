module AST 
  ( module AST
  , module Common.AST
  ) where

import qualified Data.Map.Strict    as M
import qualified Data.Text          as T
import Common.AST 


type Error = T.Text
type Asp   = T.Text
type VName = T.Text
type FName = T.Text
type LExp  = T.Text
type Atom  = T.Text
type User  = T.Text
type LanguageAtom = T.Text
type FileName     = T.Text 


-- Network definition. 
-- Consists of imports statements, language definition and expressions.
data Network = Network {imp :: [Import], lang :: Language, exps :: [Expression]}
    deriving (Eq, Show)

-- Import: A import holds the file to import.
data Import = Imp {file :: FileName}
  deriving (Eq, Show)

-- Language definition.
data Language = Language { langDef :: LanguageOptions }
  deriving (Eq, Show)

-- Language options are held in a map. Keyed by the aspect tag.
type LanguageOptions = M.Map ATag [ALang]

-- Expressions: The available expressions in HTPL.
data Expression = 
      EDel Atom Atom Expression
    | EIf Relation [Expression] [Expression]
    | EWhen Relation [Expression] [Expression]
    | EPolTmp Atom Expression
    | EPol Policy
    | EVar Atom
    | EValue Int
    | EImp Atom [Pred] [Expression] 
    | EGroup Atom [Atom]
    | EPred Atom Atom [Pred]
  deriving (Eq, Show)

-- Predicate: Holds the sender, receiver and expression.
data Pred = 
      Pred Atom Atom Expression
    deriving (Eq, Show)

-- Relations: The available relations in HTPL.
data Relation = 
      REval Atom Atom Expression
    | RIn Atom VName 
    | RNot Relation 
    | RSize VName Op Int
    deriving (Eq, Show)

-- Operators used in relations.
data Op = Less | Greater | Eq 
  deriving (Eq, Show)