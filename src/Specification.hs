{-# LANGUAGE OverloadedStrings #-}

module Specification (runSpecification) where

import AST as T
import Env
import qualified TPL.API as TPL
import qualified Data.Map.Strict as M
import Env (Environment)

import Common.AST
-- import Common.AST (ALang, ALangType, Type)

runSpecification :: T.Language -> Environment
-- runSpecification :: Either Import Language -> Either StaticErrors Environment
runSpecification (Language spec) = specification spec
-- runSpecification Left (Imp imp) = specification $ parseFile imp

parseFile :: T.FileName -> [T.Aspects]
parseFile = undefined

specification :: [T.Aspects] -> Environment
specification l = do makeTypeTable l


-- Creates a table of the aspect tags along with their fallback language, which in this case is always Nothing
makeTypeTable :: [T.Aspects] -> TPL.TypeTable 
makeTypeTable s = M.fromList $ extractTags s
    where 
    extractTags (Aspect tag _options : as) = (tag, (TDNST (LeafT Atomic), LBot)) : extractTags as
    extractTags [] = []

--(Tag, Nothing)
-- data Aspects = Aspect ATag [ALang]

-- makeLanguageMap :: [Aspects] -> M.Map ATag [ALang]
-- makeLanguageMap aspects = 
--     M.fromList <$> mapM extractTagAndOptions aspects
--     where
--         extractTagAndOptions (Aspect tag options) = do return (tag, options)