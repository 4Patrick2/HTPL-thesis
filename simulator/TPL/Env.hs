{-# LANGUAGE FlexibleContexts #-}

module TPL.Env
    ( module TPL.Env
    , module Control.Monad.Reader
    , module Control.Monad.Validate
    )
where

import           TPL.AST
import           TPL.Error
import           Control.Monad.Reader
import           Control.Monad.Validate
import           Data.List                 ( nub )
import qualified Data.Map.Strict           as M

import Data.Maybe

-------------------------
-- | TPL environment type
-------------------------

-- | Environment types
type TypeTable = M.Map ATag (ALangType, ALang)

specFromTTab :: TypeTable -> Spec
specFromTTab ttab =
    Spec $ map (\(x, t) -> Assoc x $ aspecFromType t) $ M.assocs ttab
  where
    aspecFromType :: (ALangType, ALang) -> Typing
    aspecFromType (tp, LBot) = Typing tp Nothing
    aspecFromType (tp, alang) = Typing tp (Just alang)

type TrustStore = M.Map Identity (M.Map Identity SuperPolicy)

getAllIdentities :: TrustStore -> [Identity]
getAllIdentities tstore = nub $ concatMap (uncurry (:))
                              $ M.toList $ M.map M.keys tstore

type Environment = (TypeTable, TrustStore)

-- | General monad for errors and environment
type ReaderError e r a = ValidateT e (Reader r) a

runReaderError :: ReaderError e r a -> r -> Either e a
runReaderError action = runReader (runValidateT action)

-- | Monad for all TPL operations
type TplEnv a = ReaderError TplErrors Environment a

runTplEnv :: TplEnv a -> Environment -> Either TplErrors a
runTplEnv = runReaderError

-- | Asks for each component
asksTTab :: (TypeTable -> a) -> TplEnv a
asksTTab f = asks (f . fst)

askTTab :: TplEnv TypeTable
askTTab = asks fst

asksTStore :: (TrustStore -> a) -> TplEnv a
asksTStore f = asks (f . snd)

askTStore :: TplEnv TrustStore
askTStore = asks snd


---------------------------
-- | Environment generation
---------------------------

generateEnvironment :: Network -> Environment
generateEnvironment (Network spec dconf) =
    (generateTTab spec, generateTStore dconf)

generateTTab :: Spec -> TypeTable
generateTTab =
    foldl (\ttab (Assoc x (Typing tp ml)) ->
            M.insert x (tp, fromMaybe LBot ml) ttab
        ) M.empty . assocs

generateTStore :: DelConfig -> TrustStore
generateTStore = foldl (flip delegatePrePolicies) M.empty . delegs
  where
    delegatePrePolicies (i1, i2, ppol) =
        delegateSuperPolicy (i1, i2, fromPrePolicies ppol)

delegateSuperPolicy :: (Identity, Identity, SuperPolicy) -> TrustStore -> TrustStore
delegateSuperPolicy (i1, i2, pol) tstore
        -- | i1 /= i2  = M.insertWith const i1 (M.singleton i2 pol) tstore
        | i1 /= i2  = M.insertWith (M.unionWith (<>)) i1 (M.singleton i2 pol) tstore
        | otherwise = tstore


----------------------------------
-- | Utilities for TPL environment
----------------------------------

getATags :: TplEnv [ATag]
getATags = asksTTab M.keys