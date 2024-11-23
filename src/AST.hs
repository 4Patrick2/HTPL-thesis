
module AST (module AST) where

import qualified Data.Map.Strict    as M
import qualified Data.Text          as T

-- newtype SuperPolicy = SuperPolicy {policies :: [Policy]}

-- data Policy = 
--     Policy Aspects 
--     | PLow | PHigh
--     deriving (Eq, Show, Read)

-- type Aspects = M.Map ATag ALang

-- data Type =
--     Atomic Ident
--     | LLow | LHigh
--     deriving (Eq, Show, Read)

-- type Ident    = T.Text
-- type Idendity = T.Text


-- type Program = [Stmt]


-- data Stmt = 
--       SImp FName
--     | SLan Asp [LExp]
--     | SExp Exp
--     deriving (Eq, Show, Read)


-- data Exp =
--       EDel VName VName Degree Exp
--     | EIf Relation [Stmt] [Stmt] 
--     | EWhen Relation [Stmt] [Stmt]
--     | EPolTmp VName Exp
--     | EPol [Pol]
--     | EVar VName
--     | EValue Int
--     | EImp VName Relation [Stmt] 
--     | EPred VName [Pred]
--     | EGroup VName [VName]
--     deriving (Eq, Show, Read)

data Pol = 
      Pol Asp ALang
    deriving (Eq, Show, Read)



data Relation = 
      REval Atom Atom Expression
    | RIn Atom VName 
    | RNot Relation 
    | RSize VName Op Int
    deriving (Eq, Show, Read)

data Op = Less | Greater | Eq 
  deriving (Eq, Show, Read)

-- data Degree = High | Low
--     deriving (Eq, Show, Read)

type Error = T.Text

type Asp   = T.Text

type VName = T.Text
type FName = T.Text
type LExp  = T.Text
type Atom  = T.Text


--------------
-- data Network = Network {imp :: Import, lang :: Language, exps :: Expressions}
--   deriving (Eq, Show)

-- data Network = Network {imp :: [Import], lang :: Language}
data Network = Network {imp :: [Import], lang :: Language, exps :: [Expression]}
  deriving (Eq, Show, Read)

data Import = Imp {file :: FileName}
  deriving (Eq, Show, Read)

data Language = Language { langDef :: [Aspects] }
  deriving (Eq, Show, Read)

data Expression = 
      EDel Atom Atom Degree Expression
    | EIf Relation [Expression] [Expression]
    | EWhen Relation [Expression] [Expression]
    | EPolTmp Atom Expression
    | EPol [Pol]
    | EVar Atom
    | EValue Int
    | EImp Atom Relation [Expression] 
    | EPred Atom [Pred]
    | EGroup Atom [Atom]
  deriving (Eq, Show, Read)

-- type LangDef  = M.Map ATag ALang
type FileName = T.Text -- File names
type ATag     = T.Text -- Aspect Tags

-- Only allow type TDNS(Atomic) or Bot/Top
data ALang = 
    TDNS Lang Lang Lang
    deriving (Eq, Show, Read)

data Degree = High | Low
  deriving (Eq, Show, Read)

data Lang = 
    Atom T.Text
  | Degree Degree
  deriving (Eq, Show, Read)

data Aspects = Aspect ATag [ALang]
  deriving (Eq, Show, Read)

data Pred = 
      Pred Atom Atom Expression
    deriving (Eq, Show, Read)