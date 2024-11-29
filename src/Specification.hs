{-# LANGUAGE OverloadedStrings #-}

module Specification (runSpecification) where

import AST as T
import Env
import qualified TPL.API as TPL
import qualified Data.Map.Strict as M
import Common.AST 
import AST (LanguageOptions)

runSpecification :: T.Language -> Environment
-- runSpecification :: Either Import Language -> Either StaticErrors Environment
runSpecification (Language spec) = specification spec
-- runSpecification Left (Imp imp) = specification $ parseFile imp

parseFile :: T.FileName -> [T.Aspects]
parseFile = undefined

-- specification :: [T.Aspects] -> Environment
-- specification l = (languageOptions l, makeTypeTable l)
specification :: LanguageOptions -> Environment
specification l = (l, makeTypeTable l)

-- Creates a table of the aspect tags along with their fallback language, which in this case is always Nothing
-- makeTypeTable :: [T.Aspects] -> M.Map Common.AST.ATag (Common.AST.ALangType, Common.AST.ALang)-- TPL.TypeTable 
-- makeTypeTable s = M.fromList $ extractTags s
--     where 
--     extractTags (Aspect tag _options : as) = (tag, (TDNST (LeafT Atomic), LBot)) : extractTags as
--     extractTags [] = []
makeTypeTable :: LanguageOptions -> M.Map Common.AST.ATag (Common.AST.ALangType, Common.AST.ALang)-- TPL.TypeTable 
makeTypeTable s = M.fromList $ extractTags $ M.keys s
    where 
    extractTags (tag : tags) = (tag, (TDNST (LeafT Atomic), Common.AST.LBot)) : extractTags tags
    extractTags [] = []


-- languageOptions :: [T.Aspects] -> LanguageOptions
-- languageOptions s = M.fromList $ convertAspects s
--     where
--     convertAspects (Aspect tag options : as) = (tag, options) : convertAspects as
--     convertAspects [] = []

--(Tag, Nothing)
-- data Aspects = Aspect ATag [ALang]

-- makeLanguageMap :: [Aspects] -> M.Map ATag [ALang]
-- makeLanguageMap aspects = 
--     M.fromList <$> mapM extractTagAndOptions aspects
--     where
--         extractTagAndOptions (Aspect tag options) = do return (tag, options)