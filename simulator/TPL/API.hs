{-# LANGUAGE ApplicativeDo #-}

module TPL.API
        ( performDelegation
        , performAuthorization
        , performComputation
        , runPrePoliciesCheck
        , specFromTTab
        , getAllIdentities
        , TypeTable
        , TrustStore
        , TrustPath
        , TrustPaths
        ) where

import TPL.AST
import TPL.Error
import TPL.Env
import TPL.StaticCheck
import TPL.GraphTraversal
import Common.Reduce

-----------------------------------------------------
-- | API functions for interacting with the simulator
-----------------------------------------------------

type Result a = Either TplErrors a

performDelegation :: Delegation -> TrustStore
                    -> TypeTable -> Result TrustStore
performDelegation (i1, i2, ppol) tstore ttab = do
    pol <- runPrePoliciesCheck ppol ttab
    return $ delegateSuperPolicy (i1, i2, reduceSuperPolicy pol) tstore

performAuthorization :: Bool -> Authorization -> TypeTable
                        -> TrustStore -> Result [TrustPath]
performAuthorization stopAtFirst (i1, i2, pol) ttab tstore = do
    runFindTrustPaths stopAtFirst i1 i2 (reduceSuperPolicy pol) (ttab, tstore)

performComputation :: Node -> Node -> TypeTable
                    -> TrustStore -> Result SuperPolicy
performComputation i1 i2 ttab tstore =
    reduceSuperPolicy <$> runComputeEffectiveTrust i1 i2 (ttab, tstore)