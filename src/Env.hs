{-# LANGUAGE FlexibleContexts #-}

module Env where

import AST
import qualified TPL.API as TPL
import qualified Data.Map.Strict as M
import Control.Monad.Reader
import Control.Monad.State.Lazy
import Control.Monad.Validate
import Control.Monad.Except

data Value = 
      VGroup [Atom]
    | VPol Policy
    | VVar Atom
    deriving (Eq, Show, Read)

type Bindings = M.Map Atom Value
type Environment = (LanguageOptions, TPL.TypeTable)
-- type Environment = (Bindings, TPL.TypeTable)




--------- MISSING: Taking from lars -------------------
-- Validate = errors _X
-- Reader = Read only environment _B
-- State = the state of bindings _B
-- type ReaderStateError e r b a = ValidateT e (ReaderT r (State b)) a
type ReaderStateError e r b a = ExceptT e (ReaderT r (State b)) a
-- EnvironmentStateError

type RunEnv a = ReaderStateError Errors Environment Bindings a

runRuntimeEnv :: RunEnv a -> Environment
                -> Bindings -> (Either Errors a, Bindings)
runRuntimeEnv action context = 
  runState ( runReaderT (runExceptT action) context)
    -- runErrorT ( runState (runReaderT action context))
    -- runState (runReaderT (runValidateT action) context)


getTypeTable :: RunEnv TPL.TypeTable
getTypeTable = do lift $ asks snd

getLanguageOptions :: RunEnv LanguageOptions
getLanguageOptions = do lift $ asks fst


data Errors =
      NoGroup
    | NoRelation
    | NoBindingForVariable Atom
    deriving (Eq, Show, Read)
