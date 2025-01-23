{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Evaluator (runEvaluator) where

import AST
import Parser
import Env
import Data.List ( nub, delete )
import Control.Monad (foldM)
import Control.Monad.State.Lazy
import Control.Monad.Reader
import Control.Monad.Except
import TPL.API as TPL
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Data.MultiMap as MM
import GHC.Base (undefined)



type Result a = Either Errors a

runEvaluator :: Network -> Environment -> Result (Bindings, TPL.TrustStore)
runEvaluator network env = do
    let result = runRunEnv (topEvaluation (exps network)) env M.empty
        bindings = snd result
    pts <- fst result
    return (bindings, toTrustStore pts)


----- Code from TPL Simulator ------
type PreTrustStore = MM.MultiMap (Atom, Atom) SuperPolicy

-- | Simulating the delegations to be executed locally be each entity
toTrustStore :: PreTrustStore -> TPL.TrustStore
toTrustStore pstore = M.foldrWithKey ( \(i1, i2) pols acc ->
                        M.insertWith M.union i1 (M.singleton i2 $ foldl1 (<>) pols) acc
                      ) M.empty $ MM.toMap pstore
---------------------------

topEvaluation :: [Expression] -> RunEnv PreTrustStore
topEvaluation stmts = evalStatements stmts MM.empty

-- Evaluates consecuative statements. Checks for changes in "when" relations in between.
evalStatements :: [Expression] -> PreTrustStore 
                        -> RunEnv PreTrustStore
evalStatements (stmt:stmts) pts = do
    whenPts <- processWhen pts
    newPts <- evalStatement stmt whenPts
    evalStatements stmts newPts
evalStatements [] pts = processWhen pts

evalStatement :: Expression -> PreTrustStore -> RunEnv PreTrustStore
evalStatement (EIf r e1 e2) pts         = do evalIf r e1 e2 pts
evalStatement (EWhen r e1 e2) pts       = do evalWhen r e1 e2 pts
-------------------
-- evalStatement (EImp a r es) pts         = do evalFor a r es pts

evalStatement (EImp a r es) pts         = do evalForPred a r es pts
evalStatement (EFor x g es) pts         = do evalFor x g es pts
evalStatement (EForExplicit x g es) pts = do evalForExplicit x g es pts

-------------------
evalStatement (EDel user1 user2 e) pts  = do evalDelegations user1 user2 e pts
evalStatement (EGroup name members) pts = do
    group <- evalGroup members
    withBinding name group
    return pts

evalStatement (EPol ps) pts             = do
    _ <- evalPolicy (EPol ps)
    return pts

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

-- Adds binding to bindings.
withBinding :: Atom -> Value -> RunEnv ()
withBinding name val = do
    lift . modify $ M.insert name val

-- Finds a value in the bindings.
lookupBinding :: Atom -> RunEnv Value
lookupBinding a = do
    t <- gets (M.lookup a)
    case t of
        Just v -> return v
        Nothing -> throwError $ NoBindingForVariable a

-- For addition
-- -- Finds a value in the bindings.
-- lookupBindVal :: Atom -> Value
-- lookupBindVal a = do
--     t <- gets (M.lookup a)
--     case t of
--         Just v -> v
--         Nothing -> throwError $ NoBindingForVariable a

-- Add every user to user list.
evalGroup :: [Atom] -> RunEnv Value
evalGroup g = do
    addUsers g
    return $ VGroup g

evalValue :: Int -> Value
evalValue = VVal


----------------------------
--- Predicate evaluation ---
----------------------------

-- Performs a query between two idendities using the TPL simulator.
performQuery :: Atom -> Atom -> PreTrustStore -> RunEnv Value
performQuery i1 i2 pts = do
    typeTable <- getTypeTable
    case TPL.performComputation i1 i2 typeTable (toTrustStore pts) of
        Right sp -> return $ VSuperPolicy (policies sp)
        _ -> throwError $ DefaultError "Problem with query."

-- Compares a list of polices against a policy.
comparePolicies :: [Policy] -> Policy -> RunEnv Value
comparePolicies (p1:ps) (Policy p2) = do
    res <- comparePolicy p1 (Policy p2) (M.keys p2)
    (if res
        then comparePolicies ps (Policy p2)
        else return $ VBool False)
comparePolicies [] (Policy p2) = do return $ VBool True

comparePoliciesPredicate :: [Policy] -> Policy -> RunEnv Value
comparePoliciesPredicate (p1:ps) (Policy p2) = do
    res <- comparePolicy (Policy p2) p1 (M.keys p2)
    (if res
        then comparePoliciesPredicate ps (Policy p2)
        else return $ VBool False)
comparePoliciesPredicate [] (Policy p2) = do return $ VBool True

-- Compare two polices.
comparePolicy :: Policy -> Policy -> [Atom] -> RunEnv Bool
comparePolicy (Policy pol1) (Policy pol2) (key:keys) = do
    case M.lookup key pol1 of
        Nothing -> throwError $ DefaultError "No keys in map"
        Just (TDNS ( Node (Atom f1) (Leaf s1) (Leaf t1) )) -> do
            case M.lookup key pol2 of
                Nothing -> throwError $ DefaultError "No keys in map"
                Just (TDNS ( Node (Atom f2) (Leaf s2) (Leaf t2) )) -> do
                    case f1 == f2 of
                        False -> return False
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

-- Performs a query and extracts the value.
getPolicyList :: Atom -> Atom -> PreTrustStore -> RunEnv [Policy]
getPolicyList i1 i2 pts = do
    r <- performQuery i1 i2 pts
    case r of
        (VSuperPolicy res) -> return res
        _ -> throwError $ DefaultError "Query failed!"

-- Evaluates a predicate. 
-- Goes through a list of users and compares their relationship to the policy.
singlePredicate :: String -> Atom -> Policy -> [Atom] -> PreTrustStore -> RunEnv [Atom]
-- Receiver used when the variable is the receiver.
singlePredicate "receiver" i1 policy [] pts = do return []
singlePredicate "receiver" i1 policy (user:users) pts = do
    res <- getPolicyList i1 user pts
    comparison <- comparePoliciesPredicate res policy
    -- comparison <- comparePolicies policy res
    case comparison of
        VBool True -> do
            includedUsers <- singlePredicate "receiver" i1 policy users pts
            return (user:includedUsers)
        VBool False -> do
            singlePredicate "receiver" i1 policy users pts
-- Sender used when the variable is the sender.
singlePredicate "sender" i2 policy [] pts = do return []
singlePredicate "sender" i2 policy (user:users) pts = do
    res <- getPolicyList user i2 pts
    comparison <- comparePoliciesPredicate res policy
    -- comparison <- comparePolicies res policy
    case comparison of
        VBool True -> do
            includedUsers <- singlePredicate "sender" i2 policy users pts
            return (user:includedUsers)
        VBool False -> do
            singlePredicate "sender" i2 policy users pts

-- Evaluates a list of predicates. Finds users that holds for all predicates. 
evalPredicates :: Atom -> Atom -> [Pred] -> PreTrustStore -> [Atom] -> RunEnv()
evalPredicates binding a ((Pred x y (EPol pol)):preds) pts members
    | x == y    = throwError $ BadPredicate "Predicate is ill-formed. Sender and receiver must be distinct."
    | a == x    = do -- Sender
        newMembers <- singlePredicate "sender" y pol members pts
        evalPredicates binding a preds pts newMembers
    | a == y    = do -- Receiver
        newMembers <- singlePredicate "receiver" x pol members pts
        evalPredicates binding a preds pts newMembers
    | otherwise = throwError $ BadPredicate "Predicate is ill-formed"
evalPredicates binding a [] pts members = do
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
checkPolicy' [] = return True

-- Policy template expression: Checks policy and adds to state. 
evalPolicyTemplate :: Atom -> Policy -> RunEnv()
evalPolicyTemplate name policy = do
    _ <- checkPolicy policy         -- Verify policy contains language options.
    withBinding name (VPol policy)  -- Bind policy to name.

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
        Just (VUsers l) -> do       -- List already initialized
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
findPairsGroupToId group i2 = do return $ filter (\(x,y) -> x /= y) $ map (\x -> (x, i2)) group

testMap :: Atom -> Atom -> Maybe (Atom, Atom)
testMap i1 i2
    | i1 == i2 = Just (i1, i2)
    | otherwise = Nothing

-- evalDelegations :: Atom -> Atom -> Expression -> PreTrustStore -> RunEnv PreTrustStore
-- evalDelegations i1 i2 ePol pts = do
--     pol <- evalPolicy ePol
--     i1_group <- gets (M.lookup i1)
--     i2_group <- gets (M.lookup i2)
--     case i1_group of
--         -- Idendity 1 is a group
--         Just (VGroup g1) -> do
--             case i2_group of
--                 Just g2 -> throwError $ UnsupportedOperation "Groups can not delegate trust to groups!"
--                 Nothing -> do
--                     pairs <- findPairsGroupToId g1 i2
--                     return $ foldl (dele pol) pts pairs
--                     where
--                         dele pol pts (g_i1, i2) = MM.insert (g_i1, i2)  (SuperPolicy [pol]) pts

--         -- Idendity 1 is a user
--         Nothing -> do
--             addUser i1
--             case i2_group of
--                 -- Idendity 2 is a group
--                 Just (VGroup g2) -> do
--                     pairs <- findPairs i1 g2
--                     return $ foldl (dele pol) pts pairs
--                     where
--                         dele pol pts (i1,g_i2) = MM.insert (i1,g_i2) (SuperPolicy [pol]) pts
--                 -- Idendity 2 is a user
--                 Nothing -> do
--                     addUser i2
--                     return $ MM.insert (i1, i2) (SuperPolicy [pol]) pts

-- For addition
evalDelegations :: Atom -> Atom -> Expression -> PreTrustStore -> RunEnv PreTrustStore
evalDelegations i1 i2 ePol pts = do
    tmp <- createAtom "tmp"
    lookup <- gets (M.lookup tmp)
    case lookup of
        Just (VTmp x xBinding) -> do
            case deleIntermidiary x i1 i2 of
                1 -> do delegations i1 xBinding ePol pts
                2 -> do delegations xBinding i2 ePol pts
                3 -> do throwError $ DefaultError "Both values cant be variables."
                4 -> do delegations i1 i2 ePol pts
        Nothing   -> do delegations i1 i2 ePol pts

deleIntermidiary :: Atom -> Atom -> Atom -> Int
deleIntermidiary x i1 i2
    | x == i1 && x == i2 = do 3
    | x == i2 && x == i1 = do 3
    | x == i2 = do 1
    | x == i1 = do 2
    | otherwise = do 4

delegations :: Atom -> Atom -> Expression -> PreTrustStore -> RunEnv PreTrustStore
delegations i1 i2 ePol pts = do
    pol <- evalPolicy ePol
    i1_group <- gets (M.lookup i1)
    i2_group <- gets (M.lookup i2)
    case i1_group of
        -- Idendity 1 is a group
        Just (VGroup g1) -> do
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
evalRelation (RSize i1 operator int) _pts = relationSize i1 operator int

relationEval :: Atom -> Atom -> Expression -> PreTrustStore -> RunEnv Bool
relationEval i1 i2 policy pts = do
    pol <- evalPolicy policy
    query <- getPolicyList i1 i2 pts
    comparisonResult <- comparePolicies query pol
    case comparisonResult of
        VBool True  -> return True
        VBool False -> return False
        _ -> throwError $ DefaultError "Something went wrong!"


-----------------------------------------------

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
            case op of
                Less    -> return (length group <  size)
                Greater -> return (length group >  size)
                Eq      -> return (length group == size)
        _ -> throwError $ DefaultError "Variable not a group"


--------------------
--- If statement ---
--------------------

evalIf :: Relation -> [Expression] -> [Expression] -> PreTrustStore -> RunEnv PreTrustStore-- PreTrustStore
evalIf relation exps1 exps2 pts = do
    r_res <- evalRelation relation pts
    if r_res
        then (do evalStatements exps1 pts)
        else (do evalStatements exps2 pts)


----------------------
--- When Statement ---
----------------------

evalWhen :: Relation -> [Expression] -> [Expression] -> PreTrustStore -> RunEnv PreTrustStore
evalWhen relation exps1 exps2 pts = do
    r_res <- evalRelation relation pts
    new_pts <- evalIf relation exps1 exps2 pts
    key <- createAtom "when"
    whenProcess <- gets (M.lookup key)
    let w = (r_res, relation, exps1, exps2) in do
        case whenProcess of
            Just (VWhen whens) -> do
                _ <- withBinding key (VWhen (nub $ w:whens))
                return new_pts
            Nothing -> do
                _ <- withBinding key (VWhen [w])
                return new_pts



-- Checks if a "when" key exists in the bindings. Goes through values if it exists. 
processWhen :: PreTrustStore -> RunEnv PreTrustStore
processWhen pts = do
    key <- createAtom "when"
    whenProcess <- gets (M.lookup key)
    case whenProcess of
        Nothing -> return pts
        Just (VWhen whens) -> do
            whenRerun whens pts

-- Checks if the new result of the relation is different from the old. Evaluates opposite relation in that case.
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

-- For addition
changeRelation :: Relation -> Atom -> Atom -> Relation
changeRelation (RSize group op size) _a1 _a2 = do RSize group op size
changeRelation (RNot relation) _a1 _a2 = do RNot $ changeRelation relation _a1 _a2
changeRelation (RIn a group) _a1 _a2 = do RIn a group
changeRelation (REval from to exps) x val = do
    case relationIntermediary x from to of
        1 -> do REval from val exps
        2 -> do REval val to exps
        3 -> do REval from to exps

relationIntermediary :: Atom -> Atom -> Atom -> Int
relationIntermediary x from to
    | x == to   = do 1
    | x == from = do 2
    | otherwise = do 3



-- New for: Functions as for loop
evalForPred :: Atom -> [Pred] -> [Expression] -> PreTrustStore -> RunEnv PreTrustStore
evalForPred x predicates expressions pts = do
    key <- createAtom "users"
    listOfUsers <- gets (M.lookup key)
    case listOfUsers of
        Nothing -> throwError $ DefaultError "No users in system"
        Just (VUsers users) -> do
            evalPredicates x x predicates pts users
            lookupG <- gets (M.lookup x)
            case lookupG of
                Just (VGroup group) -> evalForExplicit x group expressions pts
                _ -> throwError $ DefaultError "For opration failed to find group!"



evalForExplicit :: Atom -> [Atom] -> [Expression] 
                    -> PreTrustStore -> RunEnv PreTrustStore
evalForExplicit x [] expressions pts = return pts
evalForExplicit x (m:ms) expressions pts = do
    let newExps = changeExpressions expressions x m in do
        newPts <- evalStatements newExps pts
        evalForExplicit x ms expressions newPts

evalFor :: Atom -> Atom -> [Expression] 
                        -> PreTrustStore -> RunEnv PreTrustStore
evalFor x group expressions pts = do
    members <- gets (M.lookup group)
    case members of 
        Just (VGroup g) -> evalForExplicit x g expressions pts
        _ -> throwError $ NoBindingForVariable group



--- For additions

changeExpressions :: [Expression] -> Atom -> Atom -> [Expression]
changeExpressions exps x val
  = map (\ exp -> changeExpression exp x val) exps

changeExpression :: Expression -> Atom -> Atom -> Expression
changeExpression (EDel from to exp) x val = do
    case findX x from to of
        1 -> do EDel from val exp
        2 -> do EDel val to exp
        3 -> do EDel from to exp

changeExpression (EIf rel exps1 exps2) x val =
    let newRelation = changeRelation rel x val in 
        let newExps1 = changeExpressions exps1 x val in 
            let newExps2 = changeExpressions exps2 x val in 
                EIf newRelation newExps1 newExps2

changeExpression (EWhen rel exps1 exps2) x val = 
    let newRelation = changeRelation rel x val in 
        let newExps1 = changeExpressions exps1 x val in 
            let newExps2 = changeExpressions exps2 x val in 
                EWhen newRelation newExps1 newExps2

changeExpression (EPolTmp a exp) x val = EPolTmp a exp
changeExpression (EPol pol) x val = EPol pol
changeExpression (EVar atom) x val = EVar atom
changeExpression (EValue int) x val = EValue int
changeExpression (EImp var pred exps) x val = EImp var pred exps
changeExpression (EGroup name members) x val = EGroup name members
changeExpression (EPred a1 a2 pred) x val = EPred a1 a2 pred
changeExpression (EFor var group exps) x val = 
    let newExps = changeExpressions exps x val in EFor var group newExps
changeExpression (EForExplicit var members exps) x val = 
    let newExps = changeExpressions exps x val in EForExplicit var members newExps

findX :: Atom -> Atom -> Atom -> Int
findX x from to
    | x == to   = do 1
    | x == from = do 2
    | otherwise = do 3