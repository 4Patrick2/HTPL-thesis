{-# LANGUAGE OverloadedStrings #-}

module RunEvaluator (evalTest) where

import AST
import Parser

import System.Exit (die)
import System.Environment (getArgs)
import GHC.Base (undefined)
import Data.List ( nub, delete )

import qualified Data.Map.Strict        as M
import Parser.Lexer
import Parser.ParserImpl
import Data.Void            ( Void )
import Control.Monad.Reader
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import qualified Data.Text as T

import Specification
import Env
import Evaluator
import TPL.API as TPL
import AST (SuperPolicy)
import TPL.Error as TE



data EvalError = 
        ParseError String
    |   EvaluationError Errors
    |   TPLError TE.TplErrors
    deriving (Eq, Show)


-- mergeMaps :: LanguageOptions -> LanguageOptions -> LanguageOptions
-- mergeMaps m1 m2 = do
--     -- keys <- M.keys m2
--     mergeMaps' m1 m2 (M.keys m2)


-- mergeMaps' :: LanguageOptions -> LanguageOptions -> [ATag] -> LanguageOptions
-- mergeMaps' m1 m2 (key:keys) = do
--     case M.lookup key m2 of
--         Nothing -> undefined
--         Just value2 -> do
--             case M.lookup key m1 of
--                 Nothing -> do
--                     mergeMaps' (M.insert key value2  m1) m2 keys
--                 Just value1 ->  do
--                     mergeMaps' (M.insert key (nub $ value1++value2) m1) m2 keys
-- mergeMaps' m1 _m2 [] = m1




evalTest :: String -> String -> String -> Either EvalError SuperPolicy
evalTest network id1 id2 = do
    case (runNetworkParser "" (T.pack network)) of 
        Left err ->  Left $ ParseError err
        Right parse -> 
            let (lo,tt) = runSpecification (lang parse) in
                case runEvaluator parse (lo,tt) of
                    Left err ->  Left $ EvaluationError err
                    Right (binds, ts) -> 
                        case TPL.performComputation (T.pack id1) (T.pack id2) tt ts of
                            Left err ->  Left $ TPLError err
                            Right res ->  Right res