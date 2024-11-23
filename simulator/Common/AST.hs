{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE InstanceSigs         #-}

module Common.AST
    ( module Common.AST
    ) where

import           Common.Pretty
import qualified Data.PartialOrd        as P
import qualified Data.Map.Strict        as M
import qualified Data.Text              as T

-----------------------------------
-- | Top-level AST structure of TPL
-----------------------------------

type PrePolicy   = Either Bool [(ATag, ALang)]
type PrePolicies = [PrePolicy]

-- | Pre-policy conversion
fromPrePolicies :: PrePolicies -> SuperPolicy
fromPrePolicies = SuperPolicy . map fromPrePolicy
  where
    fromPrePolicy pp = case pp of
        Left False -> PBot
        Left True -> PTop
        Right as -> Policy $ M.fromList as

toPrePolicies :: SuperPolicy -> PrePolicies
toPrePolicies spol = map toPrePolicy $ policies spol
  where
    toPrePolicy pol = case pol of
        PBot -> Left False
        PTop -> Left True
        (Policy aspects) -> Right $ M.toList aspects

newtype SuperPolicy = SuperPolicy {policies :: [Policy]}
    deriving (Show, Eq)

data Policy = Policy Aspects
            | PBot | PTop
    deriving (Show, Eq)

type Aspects = M.Map ATag ALang

data ALang = Atom Ident
           | TDNS (Tree ALang)
           | LTop | LBot
    deriving (Show, Eq)

data Tree a = Node a
              (Tree a) -- | Left tree
              (Tree a) -- | Right tree
            | Leaf a
    deriving (Show, Eq)

type ATag     = T.Text -- Aspect Tags
type Ident    = T.Text -- Identifiers
type Identity = T.Text -- Identities


------------------------------------
-- | Syntax of aspect language types
------------------------------------

-- | Add parameter for flexibility
data Type a = LeafT a | TDNST (Type a)
    deriving (Show, Eq)

-- | The standard type syntax of aspect languages
type ALangType = Type ALangTypeTerm

data ALangTypeTerm = Atomic
    deriving (Show, Eq)


-------------------
-- | Inferred types
-------------------

data InferredType = Inferred (Type InferredTypeTerm)
                  | None
    deriving (Show, Eq)

data InferredTypeTerm = AtomT | AnyT
    deriving (Show, Eq)

anyType :: InferredType
anyType = Inferred $ LeafT AnyT

data ConcreteType a = Inconsistent | Ambiguous a | Concrete a
  deriving (Show, Eq)

inferredToConcrete :: InferredType -> ConcreteType ALangType
inferredToConcrete itp = case itp of
    None -> Inconsistent
    Inferred tp -> inferredToConcrete' tp
  where
    inferredToConcrete' :: Type InferredTypeTerm -> ConcreteType ALangType
    inferredToConcrete' (TDNST tp)    = TDNST <$> inferredToConcrete' tp
    inferredToConcrete' (LeafT AtomT) = Concrete  $ LeafT Atomic
    inferredToConcrete' (LeafT AnyT)  = Ambiguous $ LeafT Atomic

concreteToInferred :: ALangType -> InferredType
concreteToInferred tp = Inferred $ concreteToInferred' tp
  where
    concreteToInferred' :: ALangType -> Type InferredTypeTerm
    concreteToInferred' tp = case tp of
        TDNST tp' -> TDNST $ concreteToInferred' tp'
        LeafT _   -> LeafT AtomT


------------------------------------
-- | Partial order of AST constructs
------------------------------------

instance P.PartialOrd Policy where
    (<=) :: Policy -> Policy -> Bool
    (Policy aspects1) <= (Policy aspects2) =
        let keys = M.keys aspects1
        in keys == M.keys aspects2
            && all (\k -> (aspects1 M.! k) P.<= (aspects2 M.! k)) keys
    PBot <= _ = True
    _ <= PTop = True
    _ <= _ = False

instance P.PartialOrd ALang where
    (<=) :: ALang -> ALang -> Bool
    (Atom i1) <= (Atom i2) = i1 == i2
    (TDNS t1) <= (TDNS t2) = t1 P.<= t2
    LBot <= _ = True
    _ <= LTop = True
    _ <= _ = False

instance P.PartialOrd a => P.PartialOrd (Tree a) where
    (<=) :: Tree a -> Tree a -> Bool
    (Node d1 b1 r1) <= (Node d2 b2 r2) =
        d1 P.<= d2 && b1 P.<= b2 && r1 P.<= r2
    (Node d1 _ _) <= (Leaf d2) = d1 P.<= d2
    (Leaf d1) <= (Leaf d2) = d1 P.<= d2
    (Leaf d1) <= (Node d2 _ _) = d1 P.< d2


---------------------
-- | Useful instances
---------------------

instance Semigroup SuperPolicy where
    (<>) :: SuperPolicy -> SuperPolicy -> SuperPolicy
    SuperPolicy ps1 <> SuperPolicy ps2 = SuperPolicy $ ps1 ++ ps2

instance Semigroup InferredType where
    (<>) :: InferredType -> InferredType -> InferredType
    None <> _ = None
    _ <> None = None
    Inferred tp1 <> Inferred tp2 = case (tp1, tp2) of
       (LeafT AtomT, LeafT _) -> Inferred $ LeafT AtomT
       (LeafT _, LeafT AtomT) -> Inferred $ LeafT AtomT
       (LeafT AnyT, LeafT AnyT) -> Inferred $ LeafT AnyT
       (TDNST tp1', TDNST tp2') ->
           case Inferred tp1' <> Inferred tp2' of
               None -> None
               Inferred tp -> Inferred $ TDNST tp
       (LeafT AnyT, TDNST tp) -> Inferred $ TDNST tp
       (TDNST tp, LeafT AnyT) -> Inferred $ TDNST tp
       _ -> None

instance Functor Tree where
    fmap :: (a -> b) -> Tree a -> Tree b
    fmap f (Node d b r) =
        Node (f d) (fmap f b) (fmap f r)
    fmap f (Leaf d) = Leaf $ f d

instance Foldable Tree where
    foldr :: (a -> b -> b) -> b -> Tree a -> b
    foldr f a (Node d b r) = foldr f (f d $ foldr f a b) r
    foldr f a (Leaf d) = f d a

instance Functor ConcreteType where
  fmap :: (a -> b) -> ConcreteType a -> ConcreteType b
  fmap _ Inconsistent = Inconsistent
  fmap f (Ambiguous a) = Ambiguous (f a)
  fmap f (Concrete a) = Concrete (f a)


---------------------------------
-- | Pretty printing of AST nodes
---------------------------------

instance {-# OVERLAPPING #-} Pretty PrePolicies where
    pretty :: PrePolicies -> String
    pretty = prettySepBy' semi

instance {-# OVERLAPPING #-} Pretty PrePolicy where
    pretty :: PrePolicy -> String
    pretty (Right aspects) =
        prettySepBy comma $ map (
            \(atag, alang) -> pretty atag <> colon <> pretty alang
        )aspects
    pretty (Left True)  = pretty LTop
    pretty (Left False) = pretty LBot

instance Pretty SuperPolicy where
    pretty :: SuperPolicy -> String
    pretty (SuperPolicy policies) =
       prettySepBy' semi policies

instance Pretty Policy where
    pretty :: Policy -> String
    pretty (Policy aspects) = pretty (Right (M.toList aspects) :: PrePolicy)
    pretty PTop = pretty LTop
    pretty PBot = pretty LBot

instance Pretty ALang where
    pretty :: ALang -> String
    pretty (Atom name) = pretty name
    pretty (TDNS t)  = braces $ pretty t
    pretty LTop = "*"
    pretty LBot = "_"

instance Pretty (Tree ALang) where
    pretty :: Tree ALang -> String
    pretty (Node l b r) =
        pretty l
        <> (if all (==LTop) b then empty
                              else (brackets . pretty) b)
        <> (if all (==LTop) r then empty
                              else dot <> pretty r)
    pretty (Leaf d) = pretty d

instance Pretty a => Pretty (Type a) where
    pretty :: Type a -> String
    pretty (LeafT a)  = pretty a
    pretty (TDNST tp) = "tdns" <> parens (pretty tp)

instance Pretty ALangTypeTerm where
    pretty :: ALangTypeTerm -> String
    pretty Atomic = "atomic"

instance Pretty InferredTypeTerm where
    pretty :: InferredTypeTerm -> String
    pretty AnyT  = "*"
    pretty AtomT = "atomic"

instance Pretty InferredType where
    pretty :: InferredType -> String
    pretty None = "_"
    pretty (Inferred tp) = pretty tp