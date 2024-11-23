{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE FlexibleInstances     #-}

module HTPL.AST
    ( module HTPL.AST
    , module Common.AST
    , Input
    ) where

import           Common.AST
import           Common.Pretty
import           Common.IOStuff     ( Input )

import           Data.List          ( nub )
import qualified Data.Map.Strict    as M
import qualified Data.Text          as T

--------------------------------------
-- | The HTPL language specification
--------------------------------------

data Spec = Spec {versioned :: Flag, incls :: [Inclusion], decls :: [Declaration]}
    deriving (Eq, Show)

data Inclusion = Inclusion {itag :: ATag, fallback :: Maybe ALang}
    deriving (Eq, Show)

data Declaration = Declaration {dtag :: ATag, ann :: Maybe ALangType, struc :: Structure}
    deriving (Eq, Show)

newtype Structure = Structure {terms :: [StrucTerm]}
    deriving (Eq, Show)

anyStruc :: Structure
anyStruc = Structure [LTopS]

data StrucTerm = AtomS Ident
               | TDNSS (Tree StrucTerm)
               | LTopS | LBotS
               | StrucVar Ident
    deriving (Eq, Show)


-------------------------
-- Relational expressions
-------------------------

data Relation a = One  a a
                | More a (Relation a)
    deriving (Eq, Show)

-- | For delegation
type DRelation = Relation DEntities

newtype DEntities = DEntities {entities :: [DEntity]}
    deriving (Eq, Show)

data DEntity = DSingle Ident
             | DAll GroupExpr
    deriving (Eq, Show)

data GroupExpr = GConst [Ident]
               | GName Ident
               | GAppend GroupExpr GroupExpr
    deriving (Eq, Show)

-- | For authorization
type ARelation = Relation AEntities

data AEntities = AAnd AEntities AEntities
               | AOr AEntities AEntities
               | AChoose NExpr GroupExpr
               | AAll GroupExpr | AAny GroupExpr
               | ASingle Ident
               | AVar Ident
    deriving (Eq, Show)

-- | Restricted
type RAEntities = [RAEntity]
data RAEntity = RVar Ident
              | RSingle Ident
    deriving (Eq, Ord, Show)
type RARelation = Relation RAEntities

type Number = Int

data NExpr = NVar Ident
           | NConst Number
           | NLen GroupExpr
           | NBinOp BinOp NExpr NExpr
    deriving (Eq, Show)

data BinOp = Plus | Minus | Times | Divide
    deriving (Eq, Show)

-- | Helper for relations
getLeft :: Relation a -> a
getLeft (One a _)  = a
getLeft (More a _) = a


--------------------------
-- | Network configuration
--------------------------

data Network = Network {spec :: Spec, lconfig :: LangConfig}
    deriving (Eq, Show)

newtype LangConfig = LangConfig {statements :: [Statement]}
    deriving (Eq, Show)

data Statement = GBinding                  Ident GroupExpr  LocalFlag
               | SPBinding                 Ident SPExpr     LocalFlag
               | NBinding                  Ident NExpr      LocalFlag
               | Delegation RestrictedFlag DRelation SPExpr LocalFlag
    deriving (Eq, Show)

type Flag           = Bool
type LocalFlag      = Flag
type RestrictedFlag = Flag

data SPExpr = SPConst PrePolicy
            | SPVar Ident
            | SPAppend SPExpr SPExpr
    deriving (Eq, Show)

data Auth a
    = Authorization ARelation a
    | AuthCon (Auth a) (Auth a)
    | AuthDis (Auth a) (Auth a)
    deriving (Eq, Show)

type PreAuthorization = Auth SPExpr
type Authorization    = Auth SuperPolicy


---------------------------
-- Commands for interfacing
---------------------------

type AuthArg = T.Text

data CliCommand = CliCommand Input CliSubCommand
    deriving (Eq, Show)

type CliSubCommand = Command AuthArg

data Command a
    = AuthorizeTrust a
    | ComputeTrust Identity Identity
    deriving (Eq, Show)

type WebCommand = Command PreAuthorization


--------------------------------------------------------
-- | Unification tables for authorization satisfiability
--------------------------------------------------------

type UniTable  = M.Map Ident Identity
type UniTables = [UniTable]
type EPair     = (RAEntity, RAEntity)
type EPairs    = [EPair]

hasSolution :: [UniTables] -> Bool
hasSolution = not . all null

hasBindings :: [UniTables] -> Bool
hasBindings = not . all (all M.null)

prettyUniTablesList :: [UniTables] -> String
prettyUniTablesList = prettyUniTables . nub . concat

prettyUniTables :: UniTables -> String
prettyUniTables = prettySepBy line . map prettyUniTable

prettyUniTable :: UniTable -> String
prettyUniTable = prettySepBy' comma . map prettyBinding . M.toList
  where
    prettyBinding (x, i) = "@" <> pretty x <+> equal <+> pretty i


---------------------
-- | Useful instances
---------------------

instance Functor Auth where
    fmap :: (a -> b) -> Auth a -> Auth b
    fmap f (Authorization rel v) = Authorization rel $ f v
    fmap f (AuthCon a1 a2) = AuthCon (fmap f a1) (fmap f a2)
    fmap f (AuthDis a1 a2) = AuthDis (fmap f a1) (fmap f a2)

instance Foldable Auth where
    foldMap :: Monoid m => (a -> m) -> Auth a -> m
    foldMap f (Authorization _rel v) = f v
    foldMap f (AuthCon a1 a2) = foldMap f a1 `mappend` foldMap f a2
    foldMap f (AuthDis a1 a2) = foldMap f a1 `mappend` foldMap f a2

instance Traversable Auth where
    traverse :: Applicative f => (a -> f b) -> Auth a -> f (Auth b)
    traverse g (Authorization rel v) = Authorization rel <$> g v
    traverse g (AuthCon a1 a2) = AuthCon <$> traverse g a1 <*> traverse g a2
    traverse g (AuthDis a1 a2) = AuthDis <$> traverse g a1 <*> traverse g a2


------------------
-- Pretty-printing
------------------

instance Pretty a => Pretty (Auth a) where
    pretty :: Auth a -> String
    pretty (Authorization arel v) = pretty arel <> ":" <+> pretty v
    pretty (AuthCon cmd1 cmd2) =
        let cmd1Str = if needsParens cmd1 then parens (pretty cmd1) else pretty cmd1
            cmd2Str = if needsParens cmd2 then parens (pretty cmd2) else pretty cmd2
        in cmd1Str <+> "&&" <+> cmd2Str
      where
        needsParens AuthDis {} = True
        needsParens _ = False
    pretty (AuthDis cmd1 cmd2) = pretty cmd1 <+> "||" <+> pretty cmd2

instance Pretty SPExpr where
    pretty :: SPExpr -> String
    pretty (SPConst ppol) = pretty ppol
    pretty (SPVar ident) = pretty $ T.unpack ident
    pretty (SPAppend pexpr1 pexpr2) =
        let pexpr1Str = pretty pexpr1
            pexpr2Str = pretty pexpr2
        in pexpr1Str <> ";" <> pexpr2Str

encloseIf :: Bool -> String -> String
encloseIf True = parens
encloseIf False = id

instance Pretty GroupExpr where
    pretty :: GroupExpr -> String
    pretty (GConst ls) = pretty . show $ map pretty ls
    pretty (GName ident) = pretty ident
    pretty (GAppend left right) = pretty left <+> "++" <+> pretty right

instance Pretty e => Pretty (Relation e) where
    pretty :: Relation e -> String
    pretty (One es1 es2) = pretty es1 <+> "->" <+> pretty es2
    pretty (More es rel) = pretty es <+> "->" <+> pretty rel

instance Pretty DEntities where
    pretty :: DEntities -> String
    pretty dens = prettySepBy comma $ entities dens

instance Pretty DEntity where
    pretty :: DEntity -> String
    pretty (DSingle i) = pretty i
    pretty (DAll gexpr) = "all(" <> pretty gexpr <> ")"

instance Pretty AEntities where
    pretty :: AEntities -> String
    pretty (ASingle ident) = pretty $ T.unpack ident
    pretty (AChoose nExpr groupExpr) = "choose" <> parens (pretty nExpr <> "," <> pretty groupExpr)
    pretty (AAll groupExpr) = "all" <> parens (pretty groupExpr)
    pretty (AAny groupExpr) = "any" <> parens (pretty groupExpr)
    pretty (AAnd e1 e2) =
        let e1Str = if needsParens e1 then parens (pretty e1) else pretty e1
            e2Str = if needsParens e2 then parens (pretty e2) else pretty e2
        in e1Str <> "," <> e2Str
      where
        needsParens AOr {} = True
        needsParens _ = False
    pretty (AOr e1 e2) = pretty e1 <> ";" <> pretty e2
    pretty (AVar x) = "@" <> pretty x

instance Pretty NExpr where
    pretty :: NExpr -> String
    pretty (NVar ident) = pretty ident
    pretty (NConst w)   = pretty w
    pretty (NLen gexpr) = "size(" <> pretty gexpr <> ")"
    pretty (NBinOp op a b) =
        let opStr = pretty $ opToString op
            aStr = if needsParens a op then parens (pretty a) else pretty a
            bStr = if needsParens b op then parens (pretty b) else pretty b
        in aStr <+> opStr <+> bStr
      where
        opToString :: BinOp -> String
        opToString Plus   = "+"
        opToString Minus  = "-"
        opToString Times  = "*"
        opToString Divide = "/"

        needsParens :: NExpr -> BinOp -> Bool
        needsParens (NBinOp op' _ _) op = nExprPrecedence op' < nExprPrecedence op
        needsParens _ _                 = False

        nExprPrecedence :: BinOp -> Int
        nExprPrecedence Times  = 2
        nExprPrecedence Divide = 2
        nExprPrecedence Plus   = 1
        nExprPrecedence Minus  = 1

instance Pretty Structure where
    pretty :: Structure -> String
    pretty (Structure terms) = prettyPadSepBy "|" terms

instance Pretty StrucTerm where
    pretty :: StrucTerm -> String
    pretty (AtomS x) = pretty x
    pretty (TDNSS t) = braces $ pretty t
    pretty LTopS = "*"
    pretty LBotS = "_"
    pretty (StrucVar x) = "<" <> pretty x <> ">"

instance Pretty (Tree StrucTerm) where
    pretty :: Tree StrucTerm -> String
    pretty (Node l b r) =
        pretty l
        <> (if all (==LBotS) b then empty
                               else (brackets . pretty) b)
        <> (if all (==LBotS) r then empty
                               else dot <> pretty r)
    pretty (Leaf d) = pretty d

instance Pretty a => Pretty (Command a) where
    pretty :: Command a -> String
    pretty (AuthorizeTrust a) = pretty a
    pretty (ComputeTrust i1 i2) = pretty i1 <+> "=>" <+> pretty i2