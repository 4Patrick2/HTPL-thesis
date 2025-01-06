{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import AST
import Parser
import Errors
import System.Exit (die)
import System.Environment (getArgs)
import Parser.Lexer
import Control.Monad.Reader
import qualified Data.Text as T
import Specification
import Evaluator
import TPL.API as TPL

main :: IO()
main = do
    args <- getArgs
    case args of
        [f, id1, id2] -> do
            s <- readFile f
            case (runImportParser "" (T.pack s)) of
                Left err -> do putStrLn err
                Right [] -> do 
                    case (runNetworkParser "" (T.pack s)) of
                        Left err -> do putStrLn err
                        Right parse ->
                            let (lo,tt) = runSpecification (lang parse) in
                                case runEvaluator parse (lo,tt) of
                                    Left err -> do putStrLn $ printErrors err
                                    Right (_binds, ts) ->
                                        case TPL.performComputation (T.pack id1) (T.pack id2) tt ts of
                                            Left err -> print err
                                            Right res -> do putStrLn (printSuperPolicy res)
                Right imps -> do -- Handle import statements
                    let files = map (\(Imp {file=f_}) -> readFile (T.unpack f_)) imps
                    ss <- foldl (\s1 s2 -> liftM2 (++) s1 s2) (head files) (tail files)
                    case (runLOParser "" (T.pack ss)) of
                        Left err -> do putStrLn err
                        Right lo_imp -> do
                            case (runNetworkParser "" (T.pack s)) of
                                Left err -> do putStrLn err
                                Right parse ->
                                    let merged = mergeMaps (langDef (lang parse)) lo_imp in
                                    let (lo,tt) = runSpecification (Language {langDef = merged}) in
                                        case runEvaluator parse (lo,tt) of
                                            Left err -> do putStrLn $ printErrors err
                                            Right (_binds, ts) ->
                                                case TPL.performComputation (T.pack id1) (T.pack id2) tt ts of
                                                    Left err -> print err
                                                    Right res -> do putStrLn (printSuperPolicy res)
        _ -> die "Usage:\n\
                \ htpl file.htpl id1 id2"




