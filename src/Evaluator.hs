{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Evaluator (runEvaluator) where

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
import Data.List ( nub, delete )
import qualified Data.Text              as T



type Result a = Either Errors a

runEvaluator :: Network -> Environment -> Result (Bindings, TPL.TrustStore)
runEvaluator network env = do
    let result = runRuntimeEnv (topEvaluation (exps network)) env M.empty
        bindings = snd result
    pts <- fst result
    return (bindings, toTrustStore pts)





----- Code from lars ------
type PreTrustStore = MM.MultiMap (Atom, Atom) SuperPolicy
-- | Simulating the delegations to be executed locally be each entity
toTrustStore :: PreTrustStore -> TPL.TrustStore
toTrustStore pstore = M.foldrWithKey ( \(i1, i2) pols acc ->
                        M.insertWith M.union i1 (M.singleton i2 $ foldl1 (<>) pols) acc
                      ) M.empty $ MM.toMap pstore
---------------------------

topEvaluation :: [Expression] -> RunEnv PreTrustStore
topEvaluation stmts = evalStatements stmts MM.empty


evalStatements :: [Expression] -> PreTrustStore -> RunEnv PreTrustStore
-- evalStatements stmts pts = foldM (flip evalStatement) pts stmts --processWhen pts
evalStatements (stmt:stmts) pts = do
    -- newPts <- processWhen pts
    whenPts <- processWhen pts
    newPts <- evalStatement stmt whenPts
    evalStatements stmts newPts
evalStatements [] pts = processWhen pts

evalStatement :: Expression -> PreTrustStore -> RunEnv PreTrustStore
evalStatement (EIf r e1 e2) pts         = do
    evalIf r e1 e2 pts
evalStatement (EWhen r e1 e2) pts       = do evalWhen r e1 e2 pts -- When statement
evalStatement (EImp a r es) pts         = do
    evalFor a r es pts
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

evalStatement (EPred binding a ps) pts      = do
    key <- createAtom "users"
    listOfUsers <- gets (M.lookup key)
    case listOfUsers of
        Just (VUsers users) -> do
            evalPredicates binding a ps pts users
            return pts
        Nothing -> throwError $ DefaultError "No users in system"
        _ -> throwError $ DefaultError "Something went wrong in predicate!"

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
-- Add every user to user list
evalGroup :: [Atom] -> RunEnv Value
evalGroup g = do
    addUsers g
    return $ VGroup g

evalValue :: Int -> Value
evalValue = VVal

----------------------------
--- Predicate evaluation ---
----------------------------

performQuery :: Atom -> Atom -> PreTrustStore -> RunEnv Value
performQuery i1 i2 pts = do
    typeTable <- getTypeTable
    case TPL.performComputation i1 i2 typeTable (toTrustStore pts) of
        Right sp -> return $ VSuperPolicy (policies sp)
        _ -> throwError $ DefaultError "Problem with query."

comparePolicies :: [Policy] -> Policy -> RunEnv Value--Bool
comparePolicies (p1:ps) (Policy p2) = do
    res <- comparePolicy p1 (Policy p2) (M.keys p2)
    case res of
        True -> comparePolicies ps (Policy p2)
        False -> return $ VBool False
comparePolicies [] (Policy p2) = do return $ VBool True

comparePolicy :: Policy -> Policy -> [Atom] -> RunEnv Bool
comparePolicy (Policy pol1) (Policy pol2) (key:keys) = do
    case M.lookup key pol1 of
        Nothing -> throwError $ DefaultError "No keys in map"
        Just (TDNS ( Node (Atom f1) (Leaf s1) (Leaf t1) )) -> do
            case M.lookup key pol2 of
                Nothing -> throwError $ DefaultError "No keys in map"
                Just (TDNS ( Node (Atom f2) (Leaf s2) (Leaf t2) )) -> do
                    case f1 == f2 of
                        False -> return False --False --VBool Bool
                        True -> do
                               (if (s1 == s2 || s1 == LTop) && (t1 == t2 || t1 == LTop) then
                                    comparePolicy (Policy pol1) (Policy pol2) keys
                                else return False)
                        _ -> throwError  BadComparison
                _ -> throwError  BadComparison
        _ -> throwError  BadComparison
comparePolicy (Policy pol1) (Policy pol2) [] = return True
comparePolicy p1 p2 keys
    | p1 == p2 = return True
    | p1 == PTop && p2 == PBot = return True
    | p1 == PBot && p2 == PTop = return False
    | otherwise = return False

-- MISSING: Handle no result
getPolicyList :: Atom -> Atom -> PreTrustStore -> RunEnv [Policy]
getPolicyList i1 i2 pts = do
    r <- performQuery i1 i2 pts
    case r of
        (VSuperPolicy res) -> return res
        _ -> throwError $ DefaultError "Query failed!"

singlePredicate :: String -> Atom -> Policy -> [Atom] -> PreTrustStore -> RunEnv [Atom]
-- X in {id1, X
singlePredicate "receiver" i1 policy [] pts = do return []
singlePredicate "receiver" i1 policy (user:users) pts = do
    res <- getPolicyList i1 user pts
    comparison <- comparePolicies res policy
    case comparison of
        VBool True -> do
            includedUsers <- singlePredicate "receiver" i1 policy users pts
            return (user:includedUsers)
        VBool False -> do
            singlePredicate "receiver" i1 policy users pts

singlePredicate "sender" i2 policy [] pts = do return []
singlePredicate "sender" i2 policy (user:users) pts = do
    res <- getPolicyList user i2 pts
    comparison <- comparePolicies res policy
    case comparison of
        VBool True -> do
            includedUsers <- singlePredicate "sender" i2 policy users pts
            return (user:includedUsers)
        VBool False -> do
            singlePredicate "sender" i2 policy users pts

evalPredicates :: Atom -> Atom -> [Pred] -> PreTrustStore -> [Atom] -> RunEnv()
evalPredicates binding a ((Pred x y (EPol pol)):preds) pts members
    | x == y    = throwError $ BadPredicate "Predicate is ill-formed" -- Error?
    | a == x    = do -- Sender
        newMembers <- singlePredicate "sender" y pol members pts -- If y is it then reciver?
        evalPredicates binding a preds pts newMembers
    | a == y    = do -- Receiver
        newMembers <- singlePredicate "receiver" x pol members pts
        evalPredicates binding a preds pts newMembers
    | otherwise = undefined -- ???
evalPredicates binding a [] pts members = do -- Return list or add to state using a as binding????
    -- withBinding :: Atom -> Value -> RunEnv ()
    case length members of
        0 -> throwError $ DefaultError "members empty"
        _ -> do
            withBinding binding (VGroup members)

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
checkPolicy PBot = return $ VPol PBot
checkPolicy PTop = return $ VPol PTop
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
            lift . modify $ M.insert key (VUsers (nub $ l++[user]))
        Nothing ->
            lift . modify $ M.insert key (VUsers [user])

addUsers :: [Atom] -> RunEnv()
addUsers (user:users) = do
    addUser user
    addUsers users
addUsers [] = return ()

findPairs :: Atom -> [Atom] -> RunEnv [(Atom, Atom)]
findPairs i1 group = do return $ map (\x -> (i1, x)) group

findPairsGroupToId :: [Atom] -> Atom -> RunEnv [(Atom, Atom)]
findPairsGroupToId group i2 = do return $ map (\x -> (x, i2)) group


evalDelegations :: Atom -> Atom -> Expression -> PreTrustStore -> RunEnv PreTrustStore
evalDelegations i1 i2 ePol pts = do
    pol <- evalPolicy ePol
    i1_group <- gets (M.lookup i1)
    i2_group <- gets (M.lookup i2)
    case i1_group of
        -- Idendity 1 is a group
        Just (VGroup g1) -> do
            -- throwError $ UnsupportedOperation "Groups can not delegate trust" -- TODO: Possibly implemment feature.
            case i2_group of
                Just g2 -> throwError $ UnsupportedOperation "Groups can not delegate trust to groups!"
                Nothing -> do
                    pairs <- findPairsGroupToId g1 i2
                    return $ foldl (dele pol) pts pairs
                    where
                        dele pol pts (g_i1, i2) = MM.insert (g_i1, i2)  (SuperPolicy [pol]) pts

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

-----------------
--- Relations ---
-----------------

evalRelation :: Relation -> PreTrustStore -> RunEnv Bool
evalRelation (REval i1 i2 exp) pts = do relationEval i1 i2 exp pts
evalRelation (RIn id groupName) _pts = do relationIn id groupName
evalRelation (RNot relation) pts = do
    res <- evalRelation relation pts
    return $ not res
evalRelation (RSize i1 operator int) _pts = undefined

relationEval :: Atom -> Atom -> Expression -> PreTrustStore -> RunEnv Bool
relationEval i1 i2 (EPol policy) pts = do
    query <- getPolicyList i1 i2 pts
    comparisonResult <- comparePolicies query policy
    case comparisonResult of
        VBool True  -> return True
        VBool False -> return False
        _ -> throwError $ DefaultError "Something went wrong!"
relationEval _i1 _i2 _Exp _pts = throwError $ DefaultError "Something went wrong!"

relationIn :: Atom -> VName -> RunEnv Bool
relationIn name groupName = do
    g <- lookupBinding groupName
    case g of
        VGroup group -> return $ name `elem` group
        _ -> throwError $ DefaultError "Variable not a group"

relationSize :: VName -> Op -> Int -> RunEnv Bool
relationSize groupName op size = do
    search <- lookupBinding groupName
    case search of
        VGroup group -> do
            -- group_size <- length group
            case op of
                Less    -> return (length group <  size)
                Greater -> return (length group >  size)
                Eq      -> return (length group == size)
        _ -> throwError $ DefaultError "Variable not a group"

--------------------
--- If statement ---
--------------------
-- EIf Relation [Expression] [Expression]
evalIf :: Relation -> [Expression] -> [Expression] -> PreTrustStore -> RunEnv PreTrustStore-- PreTrustStore
evalIf relation exps1 exps2 pts = do
    r_res <- evalRelation relation pts
    if r_res
        then (do evalStatements exps1 pts)
        else (do evalStatements exps2 pts)


---------------------
--- For statement ---
---------------------
evalFor :: Atom -> [Pred] -> [Expression] -> PreTrustStore -> RunEnv PreTrustStore
evalFor x predicates expressions pts = do
    key <- createAtom "users"
    listOfUsers <- gets (M.lookup key)
    case listOfUsers of
        Just (VUsers users) -> do
            evalPredicates x x predicates pts users
            evalStatements expressions pts
        Nothing -> throwError $ DefaultError "No users in system"
        _ -> throwError $ DefaultError "Something went wrong!"


----------------------
--- When Statement ---
----------------------
    -- | EIf Relation [Expression] [Expression]
    -- | EWhen Relation [Expression] [Expression]
    -- State = Tag="when" Value

-- 
evalWhen :: Relation -> [Expression] -> [Expression] -> PreTrustStore -> RunEnv PreTrustStore
evalWhen relation exps1 exps2 pts = do
    r_res <- evalRelation relation pts
    new_pts <- evalIf relation exps1 exps2 pts
    key <- createAtom "when"
    whenProcess <- gets (M.lookup key)
    let w = (r_res, relation, exps1, exps2) in do
        case whenProcess of
            Just (VWhen whens) -> do -- List already initialized
                -- lift . modify $ M.insert key (VWhen (nub $ w:whens))
                -- lift . modify $ M.insert key (VUsers (nub $ l++[user]))
                _ <- withBinding key (VWhen (nub $ w:whens))
                return new_pts
            Nothing -> do
                _ <- withBinding key (VWhen [w])
                return new_pts

processWhen :: PreTrustStore -> RunEnv PreTrustStore
processWhen pts = do
    key <- createAtom "when"
    whenProcess <- gets (M.lookup key)
    case whenProcess of
        Nothing -> return pts
        Just (VWhen whens) -> do
            whenRerun whens pts

whenRerun :: [(Bool, Relation, [Expression], [Expression])] -> PreTrustStore  -> RunEnv PreTrustStore
whenRerun ((oldResult, relation, exps1, exps2):whens) pts = do
    newResult <- evalRelation relation pts
    if oldResult == newResult 
        then 
            whenRerun whens pts -- Nothing has changed.
        else do
            key <- createAtom "when"
            _ <- withBinding key (VWhen (nub $ (newResult, relation, exps1, exps2):whens))
            new_pts <- evalIf relation exps1 exps2 pts
            whenRerun whens new_pts
whenRerun [] pts = do return pts



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