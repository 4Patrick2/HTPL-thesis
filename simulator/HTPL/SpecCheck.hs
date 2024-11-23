{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module HTPL.SpecCheck
    ( runSpecCheck
    ) where

import           HTPL.AST
import           HTPL.Env
import           HTPL.Error
import           Common.Util
import qualified Common.TypeCheck   as TP
import qualified TPL.API            as TPL
import qualified Data.Map.Strict    as M
import           Data.Maybe

----------------------------
-- | Monad for static checks
----------------------------

type InferredStrucTable = M.Map ATag (InferredType, Structure)

type SpecCheckM a = ValidateT StaticErrors (State InferredStrucTable) a

runSpecCheckM :: SpecCheckM a -> InferredStrucTable -> Either StaticErrors a
runSpecCheckM action = evalState (runValidateT action)


-----------------------
-- | Running the checks
-----------------------

runSpecCheck :: Spec -> Either StaticErrors Environment
runSpecCheck spec = runSpecCheckM (checkSpec spec) M.empty


--------------
-- | The check
--------------

checkSpec :: Spec -> SpecCheckM Environment
checkSpec (Spec _ inclusions declarations) = do
    checkDuplicatesInInclusions inclusions
    checkDuplicatesInDeclarations declarations
    makeInferredStrucTable inclusions declarations
    stab <- makeStrucTable
    ttab <- makeTypeTable inclusions
    return (stab, ttab)

checkDuplicatesInDeclarations :: [Declaration] -> SpecCheckM ()
checkDuplicatesInDeclarations declarations =
    let dups = extractDuplicates $ map dtag declarations
    in unless (null dups) (throwErrors $ map DuplicateATagInSpec dups)

checkDuplicatesInInclusions :: [Inclusion] -> SpecCheckM ()
checkDuplicatesInInclusions inclusions =
    let dups = extractDuplicates $ map itag inclusions
    in unless (null dups) (throwErrors $ map DuplicateATagInInclusions dups)


-- | Construct inferred structure table
makeInferredStrucTable :: [Inclusion] -> [Declaration]
                          -> SpecCheckM ()
makeInferredStrucTable inclusions declarations = do
    makeEntriesFromDeclarations declarations
    updateEntriesFromInclusions inclusions
    checkRefCycles
    fixInferredStrucTable

makeEntriesFromDeclarations :: [Declaration] -> SpecCheckM ()
makeEntriesFromDeclarations = mapM_ makeEntry
  where
    makeEntry (Declaration atag mtp struc) = case mtp of
        Nothing -> modify (M.insert atag (anyType, struc))
        Just tp -> modify (M.insert atag (concreteToInferred tp, struc))

updateEntriesFromInclusions :: [Inclusion] -> SpecCheckM ()
updateEntriesFromInclusions = mapM_ updateEntry
  where
    updateEntry (Inclusion atag ml) =
        case ml of
            Just l -> modify $ M.insertWith (\(itp', _) (itp, struc) ->
                        (itp <> itp', struc)
                      ) atag (TP.inferType l, anyStruc)
            Nothing -> return ()

fixInferredStrucTable :: SpecCheckM ()
fixInferredStrucTable = do
    old <- get
    updateInferredStrucTable
    new <- get
    unless (new == old) fixInferredStrucTable

updateInferredStrucTable :: SpecCheckM ()
updateInferredStrucTable = do
    atags <- gets M.keys
    mapM_ updateInferredStrucTableEntry atags
  where
    updateInferredStrucTableEntry atag = do
        (itp, struc) <- gets (M.! atag)
        itp' <- inferUnifiedType (terms struc)
        modify (M.insert atag (itp <> itp', struc))


-- | Structure type inference
inferUnifiedType :: [StrucTerm] -> SpecCheckM InferredType
inferUnifiedType = foldM (flip inferType) anyType

inferType :: StrucTerm -> InferredType -> SpecCheckM InferredType
inferType (AtomS _) itp = return $ Inferred (LeafT AtomT) <> itp
inferType (TDNSS t) itp = do
    let itp' = case itp of
            Inferred (TDNST tp') -> Inferred tp'
            Inferred (LeafT AnyT) -> itp
            _ -> None
    inferTreeType t itp' >>= \case
        None -> return None
        Inferred tp -> return $ Inferred $ TDNST tp
inferType (StrucVar atag) itp = do
    gets (M.lookup atag) >>= \case
        Just (itp', struc) ->
            let itp'' = itp <> itp'
            in modify (M.insert atag (itp'', struc)) >> return itp''
        Nothing -> throwError $ UndefinedATag atag
inferType _ itp = return itp

inferTreeType :: Tree StrucTerm -> InferredType
                -> SpecCheckM InferredType
inferTreeType (Leaf d) itp = inferType d itp
inferTreeType t@(Node d b r) itp = do
    itpd <- inferType d itp
    itpl <- inferTreeType b itp
    itpr <- inferTreeType r itp
    let itp' = itpd <> itpl <> itpr
    if itp' == itp
    then return itp'
    else inferTreeType t itp'


-- | Check for referential cycles
data Color = White | Gray | Black
type Colors = M.Map ATag Color

checkRefCycles :: SpecCheckM ()
checkRefCycles = do
    atags <- gets M.keys
    checkRefCycles' atags M.empty
  where
    checkRefCycles' [] _ = return ()
    checkRefCycles' (x:xs) colors = do
        entry <- gets $ M.lookup x
        case entry of
            Just (_, struc) -> do
                colors' <- dfs [] x struc colors
                checkRefCycles' xs colors'
            Nothing -> throwError $ UndefinedATag x

dfs :: [ATag] -> ATag -> Structure -> Colors
     -> SpecCheckM Colors
dfs path a struc colors =
    case M.lookup a colors of
        Just Black -> return colors
        Just Gray  -> throwError $ RefCycleDetected (reverse (a:path))
        Nothing -> do
            let colors' = M.insert a Gray colors
                refs = getStrucVarRefs struc
            colors'' <- foldM (\colors''' ref -> do
                entry <- gets $ M.lookup ref
                case entry of
                    Just (_, struc') -> dfs (a:path) ref struc' colors'''
                    Nothing -> throwError $ UndefinedATag ref
                ) colors' refs
            return $ M.insert a Black colors''
        Just White -> do
            let colors' = M.insert a Gray colors
                refs = getStrucVarRefs struc
            colors'' <- foldM (\colors''' ref -> do
                entry <- gets $ M.lookup ref
                case entry of
                    Just (_, struc') -> dfs (a:path) ref struc' colors'''
                    Nothing -> throwError $ UndefinedATag ref
                ) colors' refs
            return $ M.insert a Black colors''

getStrucVarRefs :: Structure -> [ATag]
getStrucVarRefs = concatMap getStrucVarRefs' . terms
  where
    getStrucVarRefs' (StrucVar a) = [a]
    getStrucVarRefs' (TDNSS st) = getStrucTreeVarRefs st
    getStrucVarRefs' _ = []

    getStrucTreeVarRefs (Node d l r) =
        getStrucVarRefs' d ++ getStrucTreeVarRefs l ++ getStrucTreeVarRefs r
    getStrucTreeVarRefs (Leaf d) = getStrucVarRefs' d


-- | Making the final structure table
makeStrucTable :: SpecCheckM StrucTable
makeStrucTable = M.insert "pin" anyStruc . M.map snd <$> get

-- | Generating the TPL type table
makeTypeTable :: [Inclusion] -> SpecCheckM TPL.TypeTable
makeTypeTable inclusions =
    M.insert "pin" (LeafT Atomic, LTop) . M.fromList <$> mapM entryFromInclusion inclusions
  where
    entryFromInclusion (Inclusion atag ml) = do
        entry <- gets $ M.lookup atag
        case entry of
            Just (itp, _) -> case inferredToConcrete itp of
                Inconsistent -> throwError $ InconsistentType atag
                Ambiguous _  -> throwError $ NonDeducableType atag
                Concrete tp  -> return (atag, (tp, fromMaybe LBot ml))
            Nothing -> throwError $ NoClue atag
