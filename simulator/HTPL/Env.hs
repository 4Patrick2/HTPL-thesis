module HTPL.Env
    ( module HTPL.Env
    , module Control.Monad.State
    , module Control.Monad.Reader
    , module Control.Monad.Validate
    ) where

import           HTPL.AST
import           HTPL.Error
import qualified TPL.API                as TPL
import           Control.Monad.Validate
import           Control.Monad.Reader
import           Control.Monad.State
import qualified Data.Map.Strict        as M

------------------
-- | General types
------------------

data Value = GroupV [Ident]
           | NumV Number
           | PolV SuperPolicy
    deriving Show

data VType = GroupT | NumT | PolT

type SymTable    = M.Map Ident (Value, Bool)
type StrucTable  = M.Map ATag Structure
type Environment = (StrucTable, TPL.TypeTable)


-------------------------------------
-- | HTPL expression evaluation monad
-------------------------------------

type ReaderStateError e r s a = ValidateT e (ReaderT r (State s)) a

type RuntimeEnv a = ReaderStateError RuntimeErrors Environment SymTable a

runRuntimeEnv :: RuntimeEnv a -> Environment
                -> SymTable -> (Either RuntimeErrors a, SymTable)
runRuntimeEnv action context =
    runState (runReaderT (runValidateT action) context)

-- | Asks for each component
asksSTab :: (StrucTable -> a) -> RuntimeEnv a
asksSTab f = asks (f . fst)

askSTab :: RuntimeEnv StrucTable
askSTab = asks fst

asksTTab :: (TPL.TypeTable -> a) -> RuntimeEnv a
asksTTab f = asks (f . snd)

askTTab :: RuntimeEnv TPL.TypeTable
askTTab = asks snd