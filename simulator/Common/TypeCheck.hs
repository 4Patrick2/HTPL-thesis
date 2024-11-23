module Common.TypeCheck
    ( checkType
    , inferType
    , inferUnifiedType
    ) where

import Common.AST

----------------------------------------------
-- | Check types of aspect languages
----------------------------------------------

checkType :: ALang -> ALangType -> Bool
checkType (Atom _) (LeafT _) = True
checkType (TDNS t) (TDNST tp) = checkTreeType t tp
checkType LTop _ = True
checkType LBot _ = True
checkType _ _ = False

checkTreeType :: Tree ALang -> ALangType -> Bool
checkTreeType (Leaf d) tp = checkType d tp
checkTreeType (Node d b r) tp =
    checkType d tp
    && checkTreeType b tp
    && checkTreeType r tp


------------------------------------
-- | Infer types of aspect languages
------------------------------------

inferUnifiedType :: [ALang] -> InferredType
inferUnifiedType = foldr ((<>) . inferType) anyType

inferType :: ALang -> InferredType
inferType l = case l of
    Atom _ -> Inferred $ LeafT AtomT
    TDNS t -> case inferTreeType t of
        None -> None
        Inferred tp -> Inferred $ TDNST tp
    _ -> Inferred $ LeafT AnyT
  where
    inferTreeType (Leaf d) = inferType d
    inferTreeType (Node d b r) =
        let tp  = inferType d
            tpl = inferTreeType b
            tpr = inferTreeType r
        in tp <> tpl <> tpr