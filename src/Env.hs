
module Env (module Env) where

import AST
import qualified TPL.API as TPL
import qualified Data.Map.Strict as M
import Control.Monad.Reader
import Control.Monad.State.Lazy
import Control.Monad.Except

data Value = 
      VGroup [Atom]
    | VPol Policy
    | VVar Atom
    | VVal Int
    | VUsers [Atom]
    | VBool Bool
    | VSuperPolicy [Policy]
    | VWhen [(Bool, Relation, [Expression], [Expression])] 
  deriving (Eq, Show)

type Bindings = M.Map Atom Value
type Environment = (LanguageOptions, TPL.TypeTable)

-- Monad: 
  -- Uses Execpt for errors, 
  -- Reader for Language Options and TypeTable.
  -- State for bindings.
type ReaderStateError e r b a = ExceptT e (ReaderT r (State b)) a

type RunEnv a = ReaderStateError Errors Environment Bindings a

runRunEnv :: RunEnv a -> Environment -> Bindings -> (Either Errors a, Bindings)
runRunEnv action context = 
  runState ( runReaderT (runExceptT action) context)


getTypeTable :: RunEnv TPL.TypeTable
getTypeTable = do lift $ asks snd

getLanguageOptions :: RunEnv LanguageOptions
getLanguageOptions = asks fst


data Errors =
      NoGroup
    | NoRelation
    | NoBindingForVariable Atom
    | NoBindingForPolicy Atom
    | UnsupportedOperation String
    | NoLanguageOption ATag ALang
    | DefaultError String
    | BadPredicate String
    | BadComparison 
    | Debug Value
    deriving (Eq, Show)
