
module AST 
  ( module AST
  , module Common.AST
  ) where

import qualified Data.Map.Strict    as M
import qualified Data.Text          as T
import Common.AST 
-- import Common.AST (SuperPolicy, Policy, Aspects, ALang, Tree, Ident)


type Error = T.Text
type Asp   = T.Text
type VName = T.Text
type FName = T.Text
type LExp  = T.Text
type Atom  = T.Text
-- type LangDef  = M.Map ATag ALang
type FileName = T.Text -- File names
-- type ATag     = T.Text -- Aspect Tags


--------------

data Network = Network {imp :: [Import], lang :: Language, exps :: [Expression]}
  -- deriving (Eq, Show, Read)
    deriving (Eq, Show)

data Import = Imp {file :: FileName}
  deriving (Eq, Show)
  -- deriving (Eq, Show, Read)

-- data Language = Language { langDef :: [Aspects] }
data Language = Language { langDef :: LanguageOptions }
  deriving (Eq, Show)
  -- deriving (Eq, Show, Read)

data Expression = 
      EDel Atom Atom Expression
    | EIf Relation [Expression] [Expression]
    | EWhen Relation [Expression] [Expression]
    | EPolTmp Atom Expression
    | EPol Policy
    | EVar Atom
    | EValue Int
    | EImp Atom [Pred] [Expression] 
    -- | EImp Atom Relation [Expression] 
    | EGroup Atom [Atom]
    | EPred Atom Atom [Pred]
  deriving (Eq, Show)
  -- deriving (Eq, Show, Read)

data Pred = 
      Pred Atom Atom Expression
    deriving (Eq, Show)
    -- deriving (Eq, Show, Read)


data Relation = 
      REval Atom Atom Expression
    | RIn Atom VName 
    | RNot Relation 
    | RSize VName Op Int
    deriving (Eq, Show)
    -- deriving (Eq, Show, Read)

data Op = Less | Greater | Eq 
  deriving (Eq, Show)
  -- deriving (Eq, Show, Read)

type LanguageOptions = M.Map ATag [ALang]

------------------------------------------------
--- Removed for compatability with simulator ---
------------------------------------------------

-- Only allow type TDNS(Atomic) or Bot/Top
-- data ALang = 
--     TDNS Lang Lang Lang
--     deriving (Eq, Show, Read)

-- data Degree = High | Low
--   deriving (Eq, Show, Read)

-- data Pol = 
--       Pol Asp ALang
--     deriving (Eq, Show, Read)

-- data Lang = 
--     Atom T.Text
--   | Degree Degree
--   deriving (Eq, Show, Read)

-- data Aspects = Aspect ATag [ALang]
--   deriving (Eq, Show, Read)

--------------------------------
--- Taken from simulator ---
--- Needed for compatability ---
--------------------------------

-------- Imported from common.ast for better compatability 

-- import Common.AST (SuperPolicy, Policy, Aspects, ALang, Tree, Ident)

-- newtype SuperPolicy = SuperPolicy {policies :: [Policy]}
--     deriving (Eq, Show, Read)

-- data Policy = Policy Aspects
--             | PBot | PTop
--     deriving (Eq, Show, Read)

-- type Aspects = M.Map ATag ALang

-- data ALang = Atom Ident
--            | TDNS (Tree ALang)
--            | LTop | LBot
--     deriving (Eq, Show, Read)

-- data Tree a = Node a
--               (Tree a) -- | Left tree
--               (Tree a) -- | Right tree
--             | Leaf a
--     deriving (Eq, Show, Read)

-- type Ident = T.Text

------------------------------------



-- -- Simulator AST

-- -- | Add parameter for flexibility
-- data Type a = LeafT a | TDNST (Type a)
--     deriving (Show, Eq, Read)

-- -- | The standard type syntax of aspect languages
-- type ALangType = Type ALangTypeTerm

-- data ALangTypeTerm = Atomic
--     deriving (Show, Eq, Read)

-- data ALang = Atom Ident
--            | TDNS (Tree ALang)
--            | LTop | LBot
--     deriving (Show, Eq, Read)

-- type Ident    = T.Text -- Identifiers

-- data Tree a = Node a
--               (Tree a) -- | Left tree
--               (Tree a) -- | Right tree
--             | Leaf a
--     deriving (Show, Eq, Read)