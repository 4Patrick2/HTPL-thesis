module TPL.LatticeOps
    ( runSuperPolicyMeets
    , runSuperPolicyJoins
    , meetSuperPolicies
    , meetSuperPolicy
    , joinSuperPolicies
    , joinSuperPolicy
    , expandSuperPolicy
    , expandPolicy
    ) where

import           TPL.Env
import           TPL.AST
import           TPL.Error
import           Data.List              ( singleton )
import qualified Data.Map.Strict as M
import qualified Data.PartialOrd as P

-------------------------------------
-- | Running some selected operations
-------------------------------------

runSuperPolicyMeets :: [SuperPolicy] -> TypeTable -> Either TplErrors SuperPolicy
runSuperPolicyMeets pols ttab = runTplEnv (meetSuperPolicies pols) (ttab, M.empty)

runSuperPolicyJoins :: [SuperPolicy] -> TypeTable -> Either TplErrors SuperPolicy
runSuperPolicyJoins pols ttab = runTplEnv (joinSuperPolicies pols) (ttab, M.empty)


---------------------
-- | Join of policies
---------------------

joinSuperPolicy :: SuperPolicy -> SuperPolicy -> TplEnv SuperPolicy
joinSuperPolicy (SuperPolicy ps1) (SuperPolicy ps2) = do
    matched <- P.maxima . concat
            <$> sequence [p1 `joinPolicy` p2 | p1 <- ps1, p2 <- ps2]
    return $ SuperPolicy matched

joinSuperPolicies :: [SuperPolicy] -> TplEnv SuperPolicy
joinSuperPolicies = foldM joinSuperPolicy (SuperPolicy [PBot])

joinPolicy :: Policy -> Policy -> TplEnv [Policy]
joinPolicy pol1 pol2 | pol1 P.<= pol2 = singleton <$> expandPolicy pol2
joinPolicy pol1 pol2 | pol2 P.<= pol1 = singleton <$> expandPolicy pol1
joinPolicy pol1 pol2 = mapM expandPolicy [pol1, pol2]


---------------------
-- | Meet of policies
---------------------

meetSuperPolicies :: [SuperPolicy] -> TplEnv SuperPolicy
meetSuperPolicies = foldM meetSuperPolicy (SuperPolicy [PTop])

meetSuperPolicy :: SuperPolicy -> SuperPolicy -> TplEnv SuperPolicy
meetSuperPolicy (SuperPolicy ps1) (SuperPolicy ps2) = do
    matched <- P.maxima <$> sequence [p1 `meetPolicy` p2 | p1 <- ps1, p2 <- ps2]
    return $ SuperPolicy matched

meetPolicy :: Policy -> Policy -> TplEnv Policy
meetPolicy _ PBot = return PBot
meetPolicy PBot _ = return PBot
meetPolicy pol PTop = expandPolicy pol
meetPolicy PTop pol = expandPolicy pol
meetPolicy (Policy aspects1) (Policy aspects2) = do
    aspects <- foldM (\as x -> do
            m <- meetALang <$> getALangFromAspects x aspects1
                       <*> getALangFromAspects x aspects2
            return $ M.insert x m as
        ) M.empty =<< getATags
    if all (==LBot) aspects
    then return PBot
    else return $ Policy aspects

meetALang :: ALang -> ALang -> ALang
meetALang l1 l2 = case (l1, l2) of
    (Atom s1, Atom s2) -> if s1 == s2 then l1 else LBot
    (TDNS t1, TDNS t2) -> maybe LBot TDNS $ t1 `meetALangTree` t2
    (LTop, l) -> l
    (l, LTop) -> l
    _ -> LBot
  where
    meetALangTree (Leaf d1) (Leaf d2) =
        case d1 `meetALang` d2 of
            LBot -> Nothing
            l -> Just $ Leaf l
    meetALangTree t (Leaf d) = Leaf d `meetALangTree` t
    meetALangTree (Leaf d) t =
        Node d (Leaf LTop) (Leaf LTop) `meetALangTree` t
    meetALangTree (Node d1 b1 r1) (Node d2 b2 r2) = do
        d <- case d1 `meetALang` d2 of
               LBot -> Nothing
               l -> Just l
        b <- b1 `meetALangTree` b2
        r <- r1 `meetALangTree` r2
        return $ Node d b r


----------------------
-- | Utility functions
----------------------

getALangFromAspects :: ATag -> Aspects -> TplEnv ALang
getALangFromAspects x aspects =
    maybe ( asksTTab (M.lookup x) >>=
            maybe (throwError $ UndefinedATag x)
                  (return . snd) )
        return
        $ M.lookup x aspects

expandSuperPolicy :: SuperPolicy -> TplEnv SuperPolicy
expandSuperPolicy (SuperPolicy policies) =
    SuperPolicy <$> mapM expandPolicy policies

expandPolicy :: Policy -> TplEnv Policy
expandPolicy (Policy aspects) = do
    aspects' <- foldM (\as x -> do
            l <- getALangFromAspects x aspects
            return $ M.insert x l as
        ) M.empty =<< getATags
    return $ Policy aspects'
expandPolicy b = return b