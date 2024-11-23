{-# LANGUAGE FlexibleContexts #-}

module TPL.StaticCheck
    ( runNetworkCheck
    , runSpecCheck
    , runPrePoliciesCheck
    , runPrePoliciesChecks
    ) where

import           TPL.AST
import           TPL.Env
import           TPL.Error
import           Common.TypeCheck
import           Common.Util
import qualified Data.Map.Strict    as M

-----------------------------
-- | Monads for static checks
-----------------------------

type StaticCheckM a = Validate TplErrors a

runStaticCheckM :: StaticCheckM a -> Either TplErrors a
runStaticCheckM = runValidate

getATagType :: ATag -> TypeTable -> StaticCheckM ALangType
getATagType x ttab =
    case M.lookup x ttab of
        Just (tp, _) -> return tp
        Nothing -> throwError $ UndefinedATag x


-----------------------
-- | Running the checks
-----------------------

runNetworkCheck :: Network -> Either TplErrors Environment
runNetworkCheck = runStaticCheckM . checkNetwork

runSpecCheck :: Spec -> Either TplErrors TypeTable
runSpecCheck = runStaticCheckM . checkSpec

runPrePoliciesCheck :: PrePolicies -> TypeTable -> Either TplErrors SuperPolicy
runPrePoliciesCheck ppols = runStaticCheckM . checkPrePolicies ppols

runPrePoliciesChecks :: [PrePolicies] -> TypeTable -> Either TplErrors [SuperPolicy]
runPrePoliciesChecks ppolss =
    runStaticCheckM . checkPrePoliciesList ppolss


----------------------
-- | The actual checks
----------------------

checkNetwork :: Network -> StaticCheckM Environment
checkNetwork network = do
    ttab <- checkSpec $ spec network
    tstore <- checkDelConfig (dconfig network) ttab
    return (ttab, tstore)

checkSpec :: Spec -> StaticCheckM TypeTable
checkSpec spec = do
    wellFormedSpec spec >> wellTypedSpec spec
    return $ generateTTab spec
  where
    wellFormedSpec spec =
        let dups = extractDuplicates $ getATagsInSpec spec
        in unless (null dups) (throwErrors $ map DuplicateATagInSpec dups)

    wellTypedSpec spec =
        foldr ((*>) . wellTypedAssoc) (pure ()) (assocs spec)

    wellTypedAssoc (Assoc x (Typing tp ml)) = case ml of
        Just l -> unless (checkType l tp) $
            throwError $ TypeMismatch x l tp $ inferType l
        Nothing -> return ()

checkDelConfig :: DelConfig -> TypeTable -> StaticCheckM TrustStore
checkDelConfig dconf ttab = do
    checkDelegations dconf ttab
    return $ generateTStore dconf

checkDelegations :: DelConfig -> TypeTable -> StaticCheckM ()
checkDelegations dconf ttab =
    foldr ((*>) . (`checkDelegation` ttab)) (pure ()) (delegs dconf)

checkDelegation :: Delegation -> TypeTable -> StaticCheckM SuperPolicy
checkDelegation (_, _, ppol) = checkPrePolicies ppol

checkPrePoliciesList :: [PrePolicies] -> TypeTable -> StaticCheckM [SuperPolicy]
checkPrePoliciesList ppolss ttab =
    mapM (`checkPrePolicies` ttab) ppolss

checkPrePolicies :: PrePolicies -> TypeTable -> StaticCheckM SuperPolicy
checkPrePolicies ppols ttab = do
    wellFormedPrePolicies ppols
    wellTypedPrePolicies ppols ttab
    return $ fromPrePolicies ppols
  where
    wellFormedPrePolicies =
        foldr ((*>) . wellFormedPrePolicy) (pure ())

    wellFormedPrePolicy (Right aspects) = do
        let dups = extractDuplicates $ map fst aspects
        unless (null dups) (throwErrors $ map DuplicateATagInPol dups)
    wellFormedPrePolicy _ = pure ()

    wellTypedPrePolicies ppols ttab =
        foldr ((*>) . (`wellTypedPrePolicy` ttab)) (pure ()) ppols

    wellTypedPrePolicy (Right aspects) ttab =
        foldr ((*>) . (`wellTypedAspect` ttab)) (pure ()) aspects
    wellTypedPrePolicy _ _ = pure ()

    wellTypedAspect (x, l) ttab = do
        tp <- getATagType x ttab
        if checkType l tp
        then pure ()
        else throwError $ TypeMismatch x l tp $ inferType l

getATagsInSpec :: Spec -> [ATag]
getATagsInSpec (Spec assocs) = map atag assocs