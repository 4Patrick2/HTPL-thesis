{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import AST
import Parser

import System.Exit (die)
import System.Environment (getArgs)
import GHC.Base (undefined)


-- main :: IO ()
-- main = undefined
-- main = do 
--     args <- getArgs
--     case args of
--         --["-i", file] -> undefined
--        -- ["-p", file] -> do
--         [file] -> do
--             s <- readFile file
--             case runNetworkParser s of
--                 Left e -> putStrLn $ "*** Parse error: " ++ show e
--                 Right p -> putStrLn $ show p
--         _ -> die "Didnt work"



------------------------------

import Parser.Lexer
import Parser.ParserImpl
import Data.Void            ( Void )
import Control.Monad.Reader
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import qualified Data.Text as T
import GHC.Base (undefined)

import Specification
import Env
import Evaluator
import TPL.API as TPL

main :: IO()
-- main = case (runImportParser "" (T.pack r)) of
--     Left err -> print err
--     Right r -> print r

main =
    case (runNetworkParser "" (T.pack test1)) of 
        Left err -> print err
        Right parse -> --do
            let (l,tt) = runSpecification (lang parse) in
            case runEvaluator parse (l,tt) of
                Left err -> print err
                Right (binds, ts) -> 
                    case TPL.performComputation (T.pack "id2") (T.pack "id3") tt ts of
                        Left err -> print err
                        Right res -> print res 

im = "import language.lan."
lan = "lang Tag {aspect1, aspect1[leaf1], aspect1.leaf2, aspect2}; lang Music {Pop, rock, rock.alternative, rock.heavy}."
expres = "trust(id1, id2) with {Tag: aspect1}; trust(id2, id3) with {Tag: aspect1[leaf1]}."
-- expres = "group One = [id1, id2, id3]; trust(id2, id3) with {Tag: aspect1[leaf1]}; trust(id1, id2) with {Tag: aspect1}."
exp_if = "group One = [id1, id2, id3]; if (id4 in One) then {trust(id1, id2) with {Tag: aspect1}.} else {trust(id1, id2) with {Tag: aspect2}.}."
exp_if_eval = "trust(id1, id2) with {Tag: aspect1}; if (eval(id1,id2: {Tag: aspect1})) then {trust(id1, id3) with {Tag: aspect1}.} else {trust(id1, id3) with {Tag: aspect2}.}."
exp_group = "group One = [id2, id3]; trust(One, id1) with {Tag: aspect1}."
exp_pred = "group One = [id2, id3]; trust(id1, One) with {Tag: aspect1}; group Two = pred X in {id1, X: {Tag: aspect1}}; trust(Two, id1) with {Tag: aspect2}."
exp_pred2 = "group One = [id2, id3]; trust(One, id1) with {Tag: aspect1}; group Two = pred X in {X, id1: {Tag: aspect1}}; trust(id1, Two) with {Tag: aspect2}."
exp_tmp = "policy Pol = {Tag: aspect1.leaf2}; trust(id1, id2) with Pol."
exp_for = "group One = [id2, id3];  trust(id1, One) with {Tag: aspect1}; for (X) where {id1, X: {Tag: aspect1}} do  {trust(X, id1) with {Tag: aspect2}.}."
exp_when_false = "when eval(id1,id2: {Tag: aspect1}) do {trust(id2, id3) with {Tag: aspect1}.} otherwise {trust(id2, id3) with {Tag: aspect2}.}."
exp_when_true = "when eval(id1,id2: {Tag: aspect1}) do {trust(id2, id3) with {Tag: aspect1}.} otherwise {trust(id2, id3) with {Tag: aspect2}; trust(id1,id2) with {Tag: aspect1}.}."
exp_trust = "trust(id2, id3) with {Tag: aspect1}."
test1 = im ++ lan ++ exp_when_true

test2 = "import file.lan. lang Tag {aspect1}. trust(id1, id2) with {Tag: aspect1}."
-- convertResult :: Pretty a => Either a b -> Either String b
-- convertResult e = case e of
--     Left err -> Left $ pretty err
--     Right res -> Right res