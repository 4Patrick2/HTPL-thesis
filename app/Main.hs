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
-- main = case (runNetworkParser "" (T.pack test1)) of
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
                    case TPL.performComputation (T.pack "id1") (T.pack "id2") tt ts of
                        Left err -> print err
                        Right res -> print res 

im = "import language.lan."
lan = "lang Tag {aspect1, aspect1[leaf1], aspect1.leaf2, aspect2}."
expres = "trust(id1, id2) with {Tag: aspect1}; trust(id2, id3) with {Tag: aspect1[leaf1]}."
-- expres = "group One = [id1, id2, id3]; trust(id2, id3) with {Tag: aspect1[leaf1]}; trust(id1, id2) with {Tag: aspect1}."
exp_if = "group One = [id1, id2, id3]; if (id4 in One) then {trust(id1, id2) with {Tag: aspect1}.} else {trust(id1, id2) with {Tag: aspect2}.}."
exp_group = "group One = [id2, id3]; trust(id1, One) with {Tag: aspect1}."
test1 = im ++ lan ++ exp_group --"  group Test = pred X in {X,a: Lanmg}."

test2 = "import file.lan. lang Tag {aspect1}. trust(id1, id2) with {Tag: aspect1}."
-- convertResult :: Pretty a => Either a b -> Either String b
-- convertResult e = case e of
--     Left err -> Left $ pretty err
--     Right res -> Right res