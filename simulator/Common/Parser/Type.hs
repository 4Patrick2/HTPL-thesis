module Common.Parser.Type
    ( Parser
    ) where

import           Data.Void            ( Void )
import           Text.Megaparsec      ( Parsec )
import           Control.Monad.Reader ( ReaderT )
import qualified Data.Text            as T

---------------------------
-- The general parser monad
---------------------------

type Parser = ReaderT [T.Text] (Parsec Void T.Text)