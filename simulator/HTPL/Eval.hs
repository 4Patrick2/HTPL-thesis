{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module HTPL.Eval
    ( runLangConfigEval
    , runAuthorizationEval
    ) where

import           HTPL.AST
import           HTPL.Env
import           HTPL.Error
import           HTPL.PolicyCheck
import           Common.Util
import           Data.List           ( nub )
import qualified TPL.API             as TPL
import qualified Data.Map.Strict     as M
import qualified Data.MultiMap       as MM

---------------------------
-- | Running the evaluation
---------------------------

type Result a = Either RuntimeErrors a

runLangConfigEval :: LangConfig -> Environment -> Result (SymTable, TPL.TrustStore)
runLangConfigEval lconfig env = do
    let result = runRuntimeEnv (evalLangConfig lconfig) env M.empty
        symtab = snd result
    pstore <- fst result
    return (symtab, toTrustStore pstore)

runAuthorizationEval :: PreAuthorization -> Environment
                    -> TPL.TrustStore -> SymTable -> Result [UniTables]
runAuthorizationEval auth env tstore symtab =
    fst $ runRuntimeEnv (evalAuthorization auth tstore) env symtab


----------------------
-- | Group expressions
----------------------

evalGroupExpr :: Bool -> GroupExpr -> RuntimeEnv [Identity]
evalGroupExpr isLocal gexpr = nub <$> evalGroupExpr' isLocal gexpr

evalGroupExpr' :: Bool -> GroupExpr -> RuntimeEnv [Identity]
evalGroupExpr' _ (GConst is) = return is
evalGroupExpr' isLocal (GName x) = do
    mg <- gets (M.lookup x)
    case mg of
        Just (GroupV is, flag)
            | not flag || isLocal -> return is
        _ -> throwError $ NoGroupWithName x
evalGroupExpr' isLocal (GAppend gexpr1 gexpr2) =
    (++) <$> evalGroupExpr' isLocal gexpr1 <*> evalGroupExpr' isLocal gexpr2


---------------------------
-- | Arithmetic expressions
---------------------------

evalNExpr :: Bool -> NExpr -> RuntimeEnv Number
evalNExpr isLocal (NVar x) = do
    symTable <- get
    case M.lookup x symTable of
        Just (NumV n, flag)
            | not flag || isLocal -> return n
        _ -> throwError $ NoNumWithName x
evalNExpr _ (NConst n) = return n
evalNExpr isLocal (NLen gexpr) = do
    is <- evalGroupExpr isLocal gexpr
    return $ length is
evalNExpr isLocal nexpr@(NBinOp bop e1 e2) = do
    n1 <- evalNExpr isLocal e1
    n2 <- evalNExpr isLocal e2
    case bop of
        Plus   -> return $ n1 + n2
        Times  -> return $ n1 * n2
        Minus  -> return $ if n2 > n1 then 0 else n1 - n2
        Divide -> if n2 == 0
                  then throwError $ DivisionByZero nexpr
                  else return (n1 `div` n2)


-------------------------
-- | Delegation relations
-------------------------

evalDRelation :: DRelation -> RuntimeEnv [(Identity, Identity)]
evalDRelation drel = case drel of
    One es1 es2 -> do
        is1 <- concat <$> mapM evalDEntity (entities es1)
        is2 <- concat <$> mapM evalDEntity (entities es2)
        return $ [(i1, i2) | i1 <- is1, i2 <- is2]
    More es drel' -> do
        is1 <- concat <$> mapM evalDEntity (entities es)
        is2 <- concat <$> mapM evalDEntity (entities $ getLeft drel')
        rest <- evalDRelation drel'
        return $ [(i1, i2) | i1 <- is1, i2 <- is2] ++ rest

evalDEntity :: DEntity -> RuntimeEnv [Identity]
evalDEntity (DSingle i) = do
    mv <- gets (M.lookup i)
    case mv of
        Just (GroupV _, _) -> throwError $ IdentityDefinedAsGroup i
        _ -> return [i]
evalDEntity (DAll gexpr) = evalGroupExpr True gexpr


----------------------------
-- | Authorization relations
----------------------------

evalARelation :: ARelation -> RuntimeEnv [[(RAEntity, RAEntity)]]
evalARelation arel = do
    rrels <- reduceARelation arel
    map nub <$> mapM evalRARelation rrels

evalRARelation :: RARelation -> RuntimeEnv [(RAEntity, RAEntity)]
evalRARelation rrel = case rrel of
    One es1 es2 -> do
        return $ [(e1, e2) | e1 <- es1, e2 <- es2]
    More es1 rrel' -> do
        let es2 = getLeft rrel'
        rest <- evalRARelation rrel'
        return $ [(e1, e2) | e1 <- es1, e2 <- es2] ++ rest

reduceARelation :: ARelation -> RuntimeEnv [RARelation]
reduceARelation (One aes1 aes2) = do
    dnf1 <- toDNF aes1
    dnf2 <- toDNF aes2
    case (dnf1, dnf2) of
        (Just aes1', Just aes2') -> return [One des1 des2 | des1 <- aes1', des2 <- aes2' ]
        _ -> return []
reduceARelation (More aes arel) = do
    dnf <- toDNF aes
    case dnf of
        Just aes' -> do
            rrels <- reduceARelation arel
            return [More des drel | des <- aes', drel <- rrels]
        _ -> reduceARelation arel

-- | Assumes DNF
splitDNF :: AEntities -> [RAEntities]
splitDNF (AOr a b) = splitDNF a ++ splitDNF b
splitDNF (AAnd a b) = [splitCon a ++ splitCon b]
splitDNF (AVar x) = [[RVar x]]
splitDNF (ASingle i) = [[RSingle i]]
splitDNF _ = undefined -- Should not happen

splitCon :: AEntities -> [RAEntity]
splitCon (AAnd a1 a2) = splitCon a1 ++ splitCon a2
splitCon (ASingle i) = [RSingle i]
splitCon (AVar x) = [RVar x]
splitCon _ = [] -- Should not happen

toDNF :: AEntities -> RuntimeEnv (Maybe [RAEntities])
toDNF aens = do
    dnf <- toDNF' aens
    case dnf of
        Nothing -> return Nothing
        Just res -> return $ Just $ splitDNF res
  where
    toDNF' :: AEntities -> RuntimeEnv (Maybe AEntities)
    toDNF' a = do
        ma' <- distribute a
        case ma' of
            Just a' ->
                if a' == a
                then return $ Just a
                else toDNF' a'
            Nothing -> return Nothing

distribute :: AEntities -> RuntimeEnv (Maybe AEntities)
distribute (AAnd (AOr a b) c) = do
    ma <- distribute (AAnd a c)
    mb <- distribute (AAnd b c)
    case (ma, mb) of
        (Just a', Just b') -> return $ Just $ AOr a' b'
        (Just a', _) -> return $ Just a'
        (_, Just b') -> return $ Just b'
        _            -> return Nothing
distribute (AAnd a (AOr b c)) = do
    ma <- distribute (AAnd a b)
    mb <- distribute (AAnd a c)
    case (ma, mb) of
        (Just a', Just b') -> return $ Just $ AOr a' b'
        (Just a', _) -> return $ Just a'
        (_, Just b') -> return $ Just b'
        _            -> return Nothing
distribute (AOr a b) = do
    ma <- distribute a
    mb <- distribute b
    case (ma, mb) of
        (Just a', Just b') -> return $ Just $ AOr a' b'
        (Just a', _) -> return $ Just a'
        (_, Just b') -> return $ Just b'
        _            -> return Nothing
distribute (AAnd a b) = do
    ma <- distribute a
    mb <- distribute b
    case (ma, mb) of
        (Just a', Just b') -> return $ Just $ AAnd a' b'
        (Just a', _) -> return $ Just a'
        (_, Just b') -> return $ Just b'
        _            -> return Nothing
distribute (AChoose nexpr gexpr) = do
    w <- evalNExpr False nexpr
    g <- evalGroupExpr False gexpr
    if w == 0 || null g
        then return Nothing
        else return $ Just
                    $ makeDis
                    $ map makeCon
                    $ combinations
                        (min w $ length g)
                        (map ASingle g)
  where
    makeCon = foldl1 AAnd
    makeDis = foldl1 AOr
distribute (AAll gexpr) = do
    g <- map ASingle <$> evalGroupExpr False gexpr
    if null g
    then return Nothing
    else return $ Just $ foldl1 AAnd g
distribute (AAny gexpr) = do
    g <- map ASingle <$> evalGroupExpr False gexpr
    if null g
    then return Nothing
    else return $ Just $ foldl1 AOr g
distribute aens = return $ Just aens


----------------------------
-- | Superpolicy expressions
----------------------------

evalSPExpr :: Bool -> SPExpr -> RuntimeEnv SuperPolicy
evalSPExpr isLocal pexpr = do
    ppol <- evalSPExpr' isLocal pexpr
    checkPrePolicies ppol

evalSPExpr' :: Bool -> SPExpr -> RuntimeEnv PrePolicies
evalSPExpr' _ (SPConst ppol) = return [ppol]
evalSPExpr' isLocal (SPVar x) = do
    mp <- gets (M.lookup x)
    case mp of
        Just (PolV pol, flag)
            | not flag || isLocal -> return $ toPrePolicies pol
        _ -> throwError $ NoPolWithName x
evalSPExpr' isLocal (SPAppend pe1 pe2) = do
    pol1 <- evalSPExpr' isLocal pe1
    pol2 <- evalSPExpr' isLocal pe2
    return $ pol1 ++ pol2


--------------------------
-- | Evaluating statements
--------------------------

type PreTrustStore = MM.MultiMap (Identity, Identity) SuperPolicy

-- | Simulating the delegations to be executed locally be each entity
toTrustStore :: PreTrustStore -> TPL.TrustStore
toTrustStore pstore = M.foldrWithKey ( \(i1, i2) pols acc ->
                        M.insertWith M.union i1 (M.singleton i2 $ foldl1 (<>) pols) acc
                      ) M.empty $ MM.toMap pstore

evalLangConfig :: LangConfig -> RuntimeEnv PreTrustStore
evalLangConfig (LangConfig stmts) = foldM (flip evalStatement) MM.empty stmts

evalStatement :: Statement -> PreTrustStore -> RuntimeEnv PreTrustStore
evalStatement (GBinding x gexpr isLocal) pstore = do
    bindVar x (evalGroupExpr True gexpr) GroupV isLocal
    return pstore
evalStatement (SPBinding x pexpr isLocal) pstore = do
    bindVar x (evalSPExpr True pexpr) PolV isLocal
    return pstore
evalStatement (NBinding x nexpr isLocal) pstore = do
    bindVar x (evalNExpr True nexpr) NumV isLocal
    return pstore
evalStatement (Delegation isRestricted drel pexpr _) pstore = do
    pairs <- evalDRelation drel
    pol   <- evalSPExpr True pexpr
    return $ foldl (delegate pol) pstore pairs
  where
    delegate pol pstore (i1, i2) =
        let pol' = if isRestricted then setPin i2 pol else pol
        in MM.insert (i1, i2) pol' pstore

setPin :: Identity -> SuperPolicy -> SuperPolicy
setPin i pol = SuperPolicy $ map (setPinInPolicy i) $ policies pol
  where
    setPinInPolicy i (Policy aspects) = Policy $ M.insert "pin" (Atom i) aspects
    setPinInPolicy _ p = p

bindVar :: Ident -> RuntimeEnv a -> (a -> Value) -> LocalFlag -> RuntimeEnv ()
bindVar x evalExpr toValue isLocal = do
    mexists <- gets (M.lookup x)
    case mexists of
        Nothing -> do
            value <- evalExpr
            modify (M.insert x (toValue value, isLocal))
        Just _ -> throwError $ VariableAlreadyBound x


---------------------------
-- | Finally, authorization
---------------------------

evalAuthorization :: PreAuthorization -> TPL.TrustStore
                    -> RuntimeEnv [UniTables]
evalAuthorization pauth tstore = do
    auth <- convertPreAuthorization pauth
    evalAuthorization' auth [[M.empty]]
  where
    evalAuthorization' (Authorization arel pol) utabss = do
        pairss <- evalARelation arel
        runAuthorization pairss pol tstore utabss
    evalAuthorization' (AuthCon auth1 auth2) utabss = do
        utabss' <- evalAuthorization' auth1 utabss
        evalAuthorization' auth2 utabss'
    evalAuthorization' (AuthDis auth1 auth2) utabss = do
        utabss'  <- evalAuthorization' auth1 utabss
        utabss'' <- evalAuthorization' auth2 utabss
        return $ utabss' ++ utabss''

    convertPreAuthorization = mapM (evalSPExpr False)

runAuthorization :: [EPairs] -> SuperPolicy -> TPL.TrustStore
                -> [UniTables] -> RuntimeEnv [UniTables]
runAuthorization _ _ _ [] = return []
runAuthorization pairss pol tstore utabss = do
    if null pairss then return utabss
    else runAuthorization' pairss pol utabss
  where
    runAuthorization' pairss pol utabss =
        filter (not . null) <$> mapM (\pairs -> do
                concat <$> mapM (evalAuthPairs pairs pol tstore) utabss
            ) pairss

evalAuthPairs :: EPairs -> SuperPolicy -> TPL.TrustStore
                    -> UniTables -> RuntimeEnv UniTables
evalAuthPairs pairs pol tstore utabs =
    foldM (evalAuthPair pol tstore) utabs pairs

evalAuthPair :: SuperPolicy -> TPL.TrustStore -> UniTables -> EPair
                -> RuntimeEnv UniTables
evalAuthPair _ _ [] _ = return []
evalAuthPair pol tstore utabs pair =
    concat <$> mapM (handlePair pol tstore pair) utabs

handlePair :: SuperPolicy -> TPL.TrustStore -> EPair -> UniTable
            -> RuntimeEnv UniTables
handlePair pol tstore (RSingle i1, RSingle i2) utab =
    handleSinglePair pol tstore i1 i2 utab
handlePair pol tstore (RSingle i, RVar x) utab =
    handleVarPair tstore (authorize pol tstore) x i utab
handlePair pol tstore (RVar x, RSingle i) utab =
    handleVarPair tstore (flip $ authorize pol tstore) x i utab
handlePair pol tstore (RVar x1, RVar x2) utab =
    handleDoubleVarPair pol tstore x1 x2 utab

handleSinglePair :: SuperPolicy -> TPL.TrustStore -> Identity
                -> Identity -> UniTable -> RuntimeEnv UniTables
handleSinglePair pol tstore i1 i2 utab = do
    pass <- authorize pol tstore i1 i2
    return [utab | pass]

handleVarPair :: TPL.TrustStore -> (Identity -> Identity -> RuntimeEnv Bool)
                -> Ident -> Identity -> UniTable -> RuntimeEnv UniTables
handleVarPair tstore authFun x i utab = do
    let is = lookupis tstore x utab
    results <- filterM (\i' -> if i' == i then return False else authFun i i') is
    return [M.insert x i' utab | i' <- results, not (isSet x i' utab)]

handleDoubleVarPair :: SuperPolicy -> TPL.TrustStore -> Ident
                    -> Ident -> UniTable -> RuntimeEnv UniTables
handleDoubleVarPair pol tstore x1 x2 utab = do
    let is1 = lookupis tstore x1 utab
        is2 = lookupis tstore x2 utab
    results <- filterM (uncurry $ authorize pol tstore)
                       [(i1, i2) | i1 <- is1, i2 <- is2]
    return [M.insert x2 i2' $ M.insert x1 i1' utab
            | (i1', i2') <- results, not (isSet x1 i1' utab || isSet x2 i2' utab)]

authorize :: SuperPolicy -> TPL.TrustStore -> Identity -> Identity
            -> RuntimeEnv Bool
authorize pol tstore i1 i2 = do
    ttab <- askTTab
    let auth = (i1, i2, setPin i2 pol)
        res  = TPL.performAuthorization False auth ttab tstore
    case res of
        Left err -> throwErrors $ map TplError $ errors err
        Right pathss -> return (not $ any (null . snd) pathss)

lookupis :: TPL.TrustStore -> Ident -> UniTable -> [Identity]
lookupis tstore x utab =
    case x `M.lookup` utab of
        Just i -> [i]
        Nothing -> TPL.getAllIdentities tstore

isSet :: Ident -> Identity -> UniTable -> Bool
isSet x i utab = i `elem` [i' | (x', i') <- M.toList utab, x' /= x]