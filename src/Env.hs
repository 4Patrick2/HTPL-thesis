module Env where

import AST
import qualified TPL.API as TPL
import qualified Data.Map.Strict as M
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Validate


data Value = 
      VGroup [Atom]
    | VPol [Pol]
    | VVar Atom
    deriving (Eq, Show, Read)

type Bindings = M.Map Atom Value
type LanguageOptions = M.Map ATag [ALang]
type Environment = (LanguageOptions, TPL.TypeTable)




--------- MISSING: Taking from lars -------------------
-- Validate = errors _X
-- Reader = Read only environment _B
-- State = the state of bindings _B
type ReaderStateError e r b a = ValidateT e (ReaderT r (State b)) a

type RunEnv a = ReaderStateError Errors Environment Bindings a

runRuntimeEnv :: RunEnv a -> Environment
                -> Bindings -> (Either RuntimeErrors a, Bindings)
runRuntimeEnv action context =
    runState (runReaderT (runValidateT action) context)


getTypeTable :: RunEnv TPL.TypeTable
getTypeTable = asks snd


data Errors =
      NoGroup
    | NoRelation
    | NoBindingForVariable
    deriving (Eq, Show, Read)
