{-# LANGUAGE OverloadedStrings #-}

module Specification (runSpecification) where

import AST
import Env
import Parser
import qualified TPL.API as TPL
import qualified Data.Map.Strict as M
import Common.AST
import Data.Text as T
import GHC.Base (undefined)
-- import Prelude as P

runSpecification :: Language -> Environment
runSpecification (Language spec) = specification spec

specification :: LanguageOptions -> Environment
specification l = (l, makeTypeTable l)

-- Creates a table of the aspect tags along with their fallback language, which in this case is always Nothing
makeTypeTable :: LanguageOptions -> M.Map ATag (ALangType, ALang)-- TPL.TypeTable 
makeTypeTable s = M.fromList $ extractTags $ M.keys s
    where
    extractTags (tag : tags) = (tag, (TDNST (LeafT Atomic), LBot)) : extractTags tags
    extractTags [] = []


-- Import statement handling 
-- Not implemented
-- parseFile :: FileName -> LanguageOptions
-- parseFile filename = do
--     (content, path) <- getContent filename
--     case runTestParser "" content of
--         Right lo -> lo
--         Left err -> undefined

-- getContent :: FileName -> IO (T.Text)
-- getContent f = do
--     c <- readFile (T.unpack f)
--     return $ T.pack c