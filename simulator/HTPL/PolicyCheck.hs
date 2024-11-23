module HTPL.PolicyCheck
    ( checkPrePolicies
    ) where

import           HTPL.AST
import           HTPL.Env
import           HTPL.Error
import qualified TPL.API            as TPL
import qualified Data.Map.Strict    as M

--------------------------
-- | Checking pre-policies
--------------------------

checkPrePolicies :: PrePolicies -> RuntimeEnv SuperPolicy
checkPrePolicies ppols = do
    ttab <- askTTab
    case TPL.runPrePoliciesCheck ppols ttab of
        Left err  -> throwErrors $ map TplError $ errors err
        Right pol -> superPolicyAdheresToStructure pol
                     >> return pol


------------------------
-- | Structure adherence
------------------------

superPolicyAdheresToStructure :: SuperPolicy -> RuntimeEnv ()
superPolicyAdheresToStructure pol =
    foldr ((*>) . policyAdheresToStructure) (pure ()) $ policies pol

policyAdheresToStructure :: Policy -> RuntimeEnv ()
policyAdheresToStructure PTop = return ()
policyAdheresToStructure PBot = return ()
policyAdheresToStructure (Policy aspects) =
    foldr ((*>) . aspectAdheresToStructure) (pure ()) $ M.toList aspects

aspectAdheresToStructure  :: (ATag, ALang) -> RuntimeEnv ()
aspectAdheresToStructure (atag, l) = do
    struc <- asksSTab (M.! atag)
    pass <- unify l struc
    unless pass $ throwError $ ALangNotAdheresTo l struc

unify :: ALang -> Structure -> RuntimeEnv Bool
unify l struc = do
    stab <- askSTab
    or <$> mapM (unify' stab l) (terms struc)
  where
    unify' stab l (StrucVar atag) = unify l $ stab M.! atag
    unify' _ (Atom x1) (AtomS x2) = return $ x1 == x2
    unify' stab (TDNS tl) (TDNSS ts) = unifyTree tl ts
      where
        unifyTree (Node l tl1 tl2) (Node struc ts1 ts2) = do
            passd  <- unify' stab l struc
            passt1 <- unifyTree tl1 ts1
            passt2 <- unifyTree tl2 ts2
            return $ passd && passt1 && passt2
        unifyTree (Leaf l) (Node struc _ _) = unify' stab l struc
        unifyTree Node {} Leaf {} = return False
        unifyTree (Leaf l) (Leaf struc) = unify' stab l struc
    unify' _ _ LTopS = return True
    unify' _ LTop _ = return True
    unify' _ _ LBotS = return False
    unify' _ LBot _ = return True
    unify' _ _ _ = return False