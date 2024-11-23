{-# LANGUAGE ScopedTypeVariables #-}

module TPL.GraphTraversal
    ( runComputeEffectiveTrust
    , runFindTrustPaths
    , Node
    , TrustPath
    , TrustPaths
    ) where

import           TPL.AST
import           TPL.Env
import           TPL.Error
import           TPL.LatticeOps
import           Data.Maybe
import qualified Data.Map.Strict as M
import qualified Data.PartialOrd as P

---------------------------
-- | Running the algorithms
---------------------------

runComputeEffectiveTrust :: Node -> Node -> Environment
                            -> Either TplErrors SuperPolicy
runComputeEffectiveTrust i1 i2 =
    runTplEnv (computeEffectiveTrust i1 i2)

runFindTrustPaths :: Bool -> Node -> Node -> SuperPolicy
                    -> Environment -> Either TplErrors TrustPaths
runFindTrustPaths stopAtFirst i1 i2 pol =
    runTplEnv (findTrustPaths stopAtFirst i1 i2 pol)


--------------------
-- | Graph traversal
--------------------

type Node       = Identity
type EdgeMap    = M.Map Node SuperPolicy
type Path       = [Node]
type Paths      = [Path]
type TrustPath  = (Policy, Paths)
type TrustPaths = [TrustPath]

computeEffectiveTrust :: Node -> Node -> TplEnv SuperPolicy
computeEffectiveTrust start target = do
    paths <- findTrustPaths False start target (SuperPolicy [PBot])
    let initialAcc = SuperPolicy [PTop]
    effectivePols <- traverse (computeEffectivePath initialAcc) (concatMap snd paths)
    if null effectivePols
    then return $ SuperPolicy [PBot]
    else joinSuperPolicies effectivePols

computeEffectivePath :: SuperPolicy -> Path -> TplEnv SuperPolicy
computeEffectivePath acc path = foldM meetAcc acc (adjacentPairs path)
  where
    meetAcc acc' (x, y) = do
      edgeMap <- getExpandedEdgeMap x
      case M.lookup y edgeMap of
        Just edgeValue -> meetSuperPolicy acc' edgeValue
        Nothing -> return acc'

getExpandedEdgeMap :: Node -> TplEnv EdgeMap
getExpandedEdgeMap node =
    mapM expandSuperPolicy . M.findWithDefault M.empty node =<< askTStore

adjacentPairs :: [a] -> [(a, a)]
adjacentPairs xs = zip xs (drop 1 xs)

findTrustPaths :: Bool -> Node -> Node -> SuperPolicy -> TplEnv [TrustPath]
findTrustPaths stopOnFirst start target pol = do
    allPaths <- forM (policies pol) $ \p -> do
        p' <- expandPolicy p
        paths <- dfsPaths start target p'
        return (p, paths)
    if stopOnFirst
        then return $ mapMaybe takeFirstPath allPaths
        else return allPaths
  where
    takeFirstPath :: TrustPath -> Maybe TrustPath
    takeFirstPath (p, paths) = case paths of
        [] -> Nothing
        _  -> Just (p, [head paths])

dfsPaths :: Node -> Node -> Policy -> TplEnv Paths
dfsPaths start end p =
    dfs start [start] [start] =<< askTStore
  where
    dfs node path visited tstore
        | node == end = return [reverse path]
        | otherwise   = do
            let neighbours = M.lookup node tstore
            neighbourPaths <- forM (M.toList $ fromMaybe M.empty neighbours) $ \(nextNode, pol) -> do
                expandedPol <- expandSuperPolicy pol
                if isNonDecreasing p expandedPol && nextNode `notElem` visited
                    then dfs nextNode (nextNode:path) (nextNode:visited) tstore
                    else return []
            return $ concat neighbourPaths

isNonDecreasing :: Policy -> SuperPolicy -> Bool
isNonDecreasing p (SuperPolicy policies) = any (p P.<=) policies