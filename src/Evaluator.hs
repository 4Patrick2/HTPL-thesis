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
import Control.Monad.Reader
import Control.Monad.Except
import GHC.Base (undefined)
import Data.List ( nub )
import qualified Data.Text              as T


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
evalStatement (EDel user1 user2 e) pts  = do 
    evalDelegations user1 user2 e pts
evalStatement (EValue v) pts            = undefined -- do return v 
evalStatement (EVar a) pts              = undefined --do lookupBinding a
-- Evaluation of a group statement. 
-- Place the group in bindings and return PreTrustStore unaltered. 
evalStatement (EGroup name members) pts     = do -- MISSING: What if group is predicate
    group <- evalGroup members
    withBinding name group
    return pts

-- policy expression: Need to check that all languages are in options
evalStatement (EPol ps) pts             = do
    _ <- evalPolicy (EPol ps)
    return pts

-- Policy template expression: Checks policy and adds to state. 
evalStatement (EPolTmp a (EPol pol)) pts = do
    evalPolicyTemplate a pol
    return pts

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
evalGroup :: [Atom] -> RunEnv Value
evalGroup g = return $ VGroup g

evalValue :: Int -> Value
evalValue = VVal

----------------------------
--- Predicate evaluation ---
----------------------------

performQuery :: Atom -> Atom -> PreTrustStore -> RunEnv SuperPolicy
performQuery i1 i2 pts = do
    -- res <- TPL.performComputation i1 i2 getTypeTable (toTrustStore pts)
    typeTable <- getTypeTable
    case TPL.performComputation i1 i2 typeTable (toTrustStore pts) of
        Right sp -> return sp
        _ -> throwError $ DefaultError "Problem with query."

comparePolicies :: [Policy] -> Policy -> Bool
comparePolicies = undefined

comparePolicy :: Policy -> Policy -> [Atom] -> RunEnv Bool
comparePolicy (Policy pol1) (Policy pol2) (key:keys) = do
    -- keys <- M.keys pol2 -- We want to check up against policy 2
    -- p1 <- M.lookup key pol1
    -- TDNS ( Node f1 (Leaf s1) (Leaf t1) ) <- M.lookup key pol1

    -- -- p2 <- M.lookup key pol2
    -- TDNS ( Node f2 (Leaf s2) (Leaf t2) ) <- M.lookup key pol2

    case M.lookup key pol1 of
        Nothing -> throwError $ DefaultError "No keys in map"
        Just (TDNS ( Node f1 (Leaf s1) (Leaf t1) )) -> do
            case M.lookup key pol2 of
                Nothing -> throwError $ DefaultError "No keys in map"
                Just (TDNS ( Node f2 (Leaf s2) (Leaf t2) )) -> do
                    case f1 == f2 of
                        False -> return False
                        True -> do
                            case s1 == s2 || s1 == LTop of -- Top or Bot???? MISSING:
                                False -> return False
                                True -> case t1 == t2 || t1 == LTop of
                                    False -> return False
                                    True -> return True

-- Pred Atom Atom Expression
-- Pred X in { X, Poul: {Tag} }
-- Pred Atom Atom Expression
evalPredicate :: Atom -> [Pred] -> PreTrustStore -> PreTrustStore -- RunEnv()
evalPredicate a (Pred x y pol:preds) pts = undefined
-- evalPredicate a (Pred x y pol:preds) pts = do
--     key <- createAtom "users"
--     userList <- gets (M.lookup key)
--     case userList of 
--         Nothing -> return pts
--         Just (VUsers l) -> undefined


-- singlePredicate :: Atom -> Pred -> [Atom] -> PreTrustStore -> [Atom]
-- singlePredicate x (Pred _x y (EPol (Policy as))) (user:users) pts = do
--     queryRes <- performQuery user y pts
--     -- evalute
--     case queryRes <= as of
--         True -> do -- Do include user
--             return $ user : singlePredicate x (Pred x y polExp) users pts
--         False -> do -- Dont include user
--             return $ singlePredicate x (Pred x y polExp) users pts

-- singlePredicate :: String -> Atom -> Policy -> [Atom] -> PreTrustStore -> [Atom]
-- singlePredicate "sender" i2 policy (user:users) pts = do
--     (SuperPolicy queryRes) <- performQuery user i2 pts
--     case comparePolicies queryRes policy of 
--         True -> do -- Do include user
--             return $ user : singlePredicate "sender" i2 policy users pts
--         False -> do -- Dont include user
--             return $ singlePredicate "sender" i2 policy users pts

--do
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

-------------------------
--- Policy Expression ---
-------------------------
evalPolicy :: Expression -> RunEnv Policy
evalPolicy (EVar p) =  do
    v <- gets (M.lookup p)
    case v of
        Just (VPol pol) ->
            return pol
        _ -> throwError $ NoBindingForPolicy p
evalPolicy (EPol p) = do
    _ <- checkPolicy p
    return p

-- Check all options are legal
-- Return policy
checkPolicy :: Policy -> RunEnv Value
checkPolicy PBot = undefined
checkPolicy PTop = undefined
checkPolicy (Policy aspects) = do
    _ <- checkPolicy' (M.toList aspects) -- Check policy and discard result. Throws error if invalid.
    return $ VPol (Policy aspects)


checkPolicy' :: [(ATag, ALang)] -> RunEnv Bool
checkPolicy' ((tag, lang):p) = do
    langOptions <- getLanguageOptions
    case M.lookup tag langOptions of
        Just options -> do
            (if lang `elem` options then (do checkPolicy' p)
             else throwError $ NoLanguageOption tag lang)
        Nothing -> throwError $ NoLanguageOption tag lang
        -- Nothing -> return False
checkPolicy' [] = return True


evalPolicyTemplate :: Atom -> Policy -> RunEnv()
evalPolicyTemplate name policy = do
    _ <- checkPolicy policy -- Verify policy contains language options.
    withBinding name (VPol policy) -- Bind policy to name.

-------------------
--- Delegations ---
-------------------

createAtom :: String -> RunEnv Atom
createAtom s = do return $ T.pack s

addUser :: Atom -> RunEnv ()
addUser user = do
    key <- createAtom "users"
    userList <- gets (M.lookup key)
    case userList of
        Just (VUsers l) -> do -- List already initialized
            lift . modify $ M.insert key (VUsers (l++[user]))
        Nothing ->
            lift . modify $ M.insert key (VUsers [user])

findPairs :: Atom -> [Atom] -> RunEnv [(Atom, Atom)]
findPairs i1 group = do return $ map (\x -> (i1, x)) group



-- MISSING: Add users to list of users! 
-- MISSING: Handle groups! 
evalDelegations :: Atom -> Atom -> Expression -> PreTrustStore -> RunEnv PreTrustStore
evalDelegations i1 i2 ePol pts = do
    pol <- evalPolicy ePol
    i1_group <- gets (M.lookup i2)
    i2_group <- gets (M.lookup i2)
    case i1_group of
        -- Idendity 1 is a group
        Just g1 -> do
            throwError $ UnsupportedOperation "Groups can not delegate trust" -- TODO: Possibly implemment feature.
        -- Idendity 1 is a user
        Nothing -> do
            addUser i1
            case i2_group of
                -- Idendity 2 is a group
                Just (VGroup g2) -> do
                    pairs <- findPairs i1 g2
                    return $ foldl (dele pol) pts pairs
                    where
                        dele pol pts (i1,g_i2) = MM.insert (i1,g_i2) (SuperPolicy [pol]) pts

                -- Idendity 2 is a user
                Nothing -> do
                    addUser i2
                    return $ MM.insert (i1, i2) (SuperPolicy [pol]) pts








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