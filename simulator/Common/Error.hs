{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE InstanceSigs      #-}

module Common.Error
    ( module Common.Error
    ) where

import      Common.Pretty
import      Data.List                   ( intersperse )
import      Control.Monad.Validate      ( MonadValidate(refute) )

-----------------------------
-- | Errors in TPL simulation
-----------------------------

newtype SomeErrors a = Errors {errors :: [a]}
    deriving (Eq, Show)

throwError :: MonadValidate (SomeErrors a1) m => a1 -> m a2
throwError err = refute . Errors $ [err]

throwErrors :: MonadValidate (SomeErrors a1) m => [a1] -> m a2
throwErrors = refute . Errors


-----------------------------------
-- | Instance for collecting errors
-----------------------------------

instance Semigroup (SomeErrors a) where
    (<>) :: SomeErrors a -> SomeErrors a -> SomeErrors a
    a <> b = Errors $ errors a ++ errors b


------------------------------
-- | Pretty-printing of errors
------------------------------

instance Pretty a => Pretty (SomeErrors a) where
    pretty :: SomeErrors a -> String
    pretty errs = prettySepBy line
                . intersperse empty
                . map (("Error:"<+>) . pretty) $ errors errs