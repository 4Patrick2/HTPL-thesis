{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Evaluator () where

import AST
import Parser
import Env
import TPL.API as TPL
import qualified Data.Map.Strict as M
import qualified Data.MultiMap       as MM
import Control.Monad (foldM)
import Control.Monad.State.Lazy
import Control.Monad.Except
import Control.Monad.Error (MonadError(throwError))


--- Running the evaluator

-- type Result a = Either RuntimeErrors a
type Result a = Either Errors Environment

runEvaluator :: Network -> Environment -> Result (Bindings, TPL.TrustStore)
runEvaluator n env = undefined
-- runEvaluator n env = do
--     let result = evalStatements (exps n)
--     return (gets , toTrustStore result)



----- Code from lars ------
-- type PreTrustStore = MM.MultiMap (Atom, Atom) Common.AST.SuperPolicy
type PreTrustStore = MM.MultiMap (Atom, Atom) SuperPolicy
-- | Simulating the delegations to be executed locally be each entity
toTrustStore :: PreTrustStore -> TPL.TrustStore
toTrustStore pstore = M.foldrWithKey ( \(i1, i2) pols acc ->
                        M.insertWith M.union i1 (M.singleton i2 $ foldl1 (<>) pols) acc
                      ) M.empty $ MM.toMap pstore
---------------------------



evalStatements :: [Expression] -> PreTrustStore
evalStatements stmts = undefined -- Go over every statement with environment

evalStatement :: Expression -> PreTrustStore -> RunEnv PreTrustStore
evalStatement (EIf r e1 e2) pts         = undefined -- if statement
evalStatement (EWhen r e1 e2) pts       = undefined -- When statement
evalStatement (EImp a r es) pts         = undefined
evalStatement (EDel user1 user2 e) pts  = undefined
evalStatement (EValue v) pts            = undefined -- do return v 
evalStatement (EVar a) pts              = undefined --do lookupBinding a
evalStatement (EGroup a as) pts         = undefined
-- evalStatement (EGroup a as) pts     = do
--     g <- evalGroup as
--     withBinding a g pts 

-- policy expression: Need to check that all languages are in options
evalStatement (EPol ps) pts             = undefined --do return ps

-- policy template
evalStatement (EPolTmp a (EPol pol)) pts = undefined
-- evalStatement (EPolTmp a (EPol pol)) pts     = do 
--     withBinding a (VPol pol)
--     return pts
evalStatement (EPred a ps) pts      = undefined

-- MISSING: Needs work
-- withBinding :: Atom -> Value -> RunEnv a -> RunEnv ()
withBinding :: Atom -> Value -> RunEnv ()
withBinding name val = do
    lift . modify $ M.insert name val


lookupBinding :: Atom -> RunEnv Value
lookupBinding a = do
    t <- gets (M.lookup a)
    case t of
        Just v -> return v
        Nothing -> throwError $ NoBindingForVariable a -- MISSING: Is this a error?

---- Evaluations
evalGroup :: [Atom] -> Value
evalGroup = VGroup

evalValue :: Int -> Value
evalValue = VVal


-- Pred Atom Atom Expression
evalPredicate :: Atom -> [Pred] -> PreTrustStore -> RunEnv()
evalPredicate a (Pred x y pol:preds) pts = undefined --do
    -- r <- TPL.performComputation a y getTypeTable (toTrustStore pts) 
    -- case r == pol of
    --     -- True ->  
    --     -- False -> 

-- evalPredicate a (Pred x a pol) pts = undefined
-- evalPredicate a (Pred x y pol) pts = undefined --Error???

-- evalPred' :: Atom -> Atom -> Pol 

-- users = [users]
-- pGroup = copy users # copy of all users 
-- predicates = [Preds]
-- for every (_ y p) in predicates do
--     for every x in users do
--         sp <- performComputation x -> y
--         if sp < p then      # result is less than required
--             remove x pGroup # Remove user from result as predicate is not met
-- return pGroup

-------------------
--- Delegations ---
-------------------
evalPolicy :: Expression -> RunEnv Policy
evalPolicy (EVar p) =  do 
    v <- gets (M.lookup p)
    case v of
        Just (VPol pol) -> return pol
        _ -> throwError $ NoBindingForPolicy p
evalPolicy (EPol p) = do return p

-- MISSING: Add users to list of users! 
-- MISSING: Handle groups! 
evalDelegations :: Atom -> Atom -> Expression -> PreTrustStore -> RunEnv PreTrustStore
evalDelegations i1 i2 ePol pts = do
    pol <- evalPolicy ePol
    user2 <- gets (M.lookup i2)
    case user2 of
        Just v -> undefined -- Its a group

        Nothing -> do-- its not a group
            -- Lig det ind i pretruststore
            -- Lav pol om til superpol -- Append til superPol eller overwrite???
            return $ MM.insert (i1, i2) (SuperPolicy [pol]) pts

-- evalStatement (EDel user1 user2 d e) pts  = do -- delegation
-- -- Every user who has made delegations gets put into a user list in the state
--     -- type Delegation = (Identity, Identity, PrePolicies)
--     -- type PrePolicy   = Either Bool [(ATag, ALang)]
--     -- type PrePolicies = [PrePolicy]
--     -- performDelegation :: Delegation -> TrustStore -> TypeTable -> Result TrustStore

--     -- add user1 to list
--     -- add user2 to list
--     -- perform delegation
--     ts <- TPL.performDelegation (user1, user2, evalStatement e) (toTrustStore pts) getTypeTable
--     -- Or place in preTrustStore
--     return ()
--     -- MISSING: Group delegations







--- Ting der skal styr på:
    -- state med variabler og 
    -- Hvad er trust store?

-- Hvad bruges TypeTable til
-- Hvad bruges TrustStore til

-- Mit Environment: Table of language options
-- Mit Environment: Variable names


-- Parse network
-- import files -> TypeTable ???
-- Language spec -> TypeTable ???

-- Evaluate expressions using typetable???
-- API Calls:
--      performDelegation  :: Delegation -> TrustStore -> TypeTable -> Result TrustStore
--      performComputation :: Node -> Node -> TypeTable -> TrustStore -> Result SuperPolicy

-- Delegation - Delegation RestrictedFlag DRelation SPExpr LocalFlag (HTPL/AST)
-- TrustStore - type TrustStore = M.Map Identity (M.Map Identity SuperPolicy) (TPL/Env)
-- Node
-- SuperPolicy - SuperPolicy {policies :: [Policy]} (Common/AST)


-- data Policy = Policy Aspects
--             | PBot | PTop
--     deriving (Show, Eq)

-- type Aspects = M.Map ATag ALang
-- type Identity = T.Text -- Identities


-- type ATag     = T.Text -- Aspect Tags

-- -- | Add parameter for flexibility
-- data Type a = LeafT a | TDNST (Type a)
--     deriving (Show, Eq)

-- -- | The standard type syntax of aspect languages
-- type ALangType = Type ALangTypeTerm

-- data ALangTypeTerm = Atomic
--     deriving (Show, Eq)

-- Min ALangType er altid
-- langtype = TDNST (LeafT Atomic)

-- type SymTable    = M.Map Ident (Value, Bool)
-- type StrucTable  = M.Map ATag Structure  --- Table of identity and the type structure i.e Tag TDNSS(Atomic)
-- type TypeTable = M.Map ATag (ALangType, ALang) --- Table of Tag and fallback language/Nothing
-- type Environment = (StrucTable, TPL.TypeTable)