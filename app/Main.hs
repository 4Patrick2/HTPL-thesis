{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

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
import AST (LanguageOptions)

main :: IO()
main = do
    args <- getArgs
    case args of
        [file, id1, id2] -> do
            s <- readFile file
            case (runImportParser "" (T.pack s)) of
                Left err -> do putStrLn err
                Right [] -> do 
                    case (runNetworkParser "" (T.pack s)) of
                        Left err -> do putStrLn err
                        Right parse ->
                            let (lo,tt) = runSpecification (lang parse) in
                                case runEvaluator parse (lo,tt) of
                                    Left err -> print err
                                    Right (binds, ts) ->
                                        case TPL.performComputation (T.pack id1) (T.pack id2) tt ts of
                                            Left err -> print err
                                            Right res -> do putStrLn (printSuperPolicy res)
                Right imps -> do
                    let files = map (\(Imp {file=f}) -> readFile (T.unpack f)) imps
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
                                            Left err -> print err
                                            Right (binds, ts) ->
                                                case TPL.performComputation (T.pack id1) (T.pack id2) tt ts of
                                                    Left err -> print err
                                                    Right res -> do putStrLn (printSuperPolicy res)
        _ -> die "Usage:\n\
                \ htpl file.htpl id1 id2"



-- main = case (runLOParser "" (T.pack impo)) of
--     Left err -> putStrLn err
--     Right r -> print r

impo = "lang Tag {t1, t2}. lang Tag2 {t3,t4}."

getImports :: Import -> IO String
getImports Imp {file = f} = do readFile (T.unpack f)

getImps :: [Import] -> [IO String]
getImps (i:is) = do
    getImports i : getImps is
getImps [] = []

parseLO :: [String] -> Either String LanguageOptions
parseLO [s] = do
    case runLOParser "" (T.pack s) of
        Left err -> Left err
        Right lo -> do Right lo
parseLO (s:ss) = do
    case runLOParser "" (T.pack s) of
        Left err -> Left err
        Right lo -> do
            case parseLO ss of
                Left err -> Left err
                Right los -> do
                    let merged = mergeMaps lo los in
                        Right merged









printSuperPolicy :: SuperPolicy -> String
printSuperPolicy (SuperPolicy pols) = do "SuperPolicy:" ++  policiesToString pols

policiesToString :: [Policy] -> String
policiesToString (p1:[]) = do policyToString p1
policiesToString (p1:pols) = do policyToString p1 ++ ", " ++ policiesToString pols



policyToString :: Policy -> String
policyToString PBot = "_"
policyToString PTop = "*"
policyToString (Policy aspects) = do "{" ++ aspectsToString (M.toList aspects) ++  "}"



aspectsToString :: [(ATag, ALang)] -> String
aspectsToString ((tag, aspect):[]) = do (T.unpack tag) ++ ": " ++ T.unpack (tdnsToString aspect)
aspectsToString ((tag, aspect):asps) = do (T.unpack tag) ++ ": " ++ T.unpack (tdnsToString aspect) ++ ", " ++ aspectsToString asps


tdnsToString :: ALang -> T.Text
tdnsToString LTop = T.pack "*"
tdnsToString LBot = T.pack "_"
tdnsToString (TDNS ( Node (Atom f) (Leaf LTop)      (Leaf LTop)))  = f
tdnsToString (TDNS ( Node (Atom f) (Leaf LTop)      (Leaf (Atom t))))   = T.pack $ getString f T.empty t
tdnsToString (TDNS ( Node (Atom f) (Leaf (Atom s))  (Leaf LTop))) = T.pack $ getString f s T.empty
tdnsToString (TDNS ( Node (Atom f) (Leaf (Atom s))  (Leaf (Atom t))))   = T.pack $ getString f s t

getString :: T.Text -> T.Text -> T.Text -> String
getString one two three
    | two == T.empty && three == T.empty = T.unpack one
    | two == T.empty = (T.unpack one ) ++ "." ++ (T.unpack three )
    | three == T.empty =  (T.unpack one ) ++ "[" ++ (T.unpack two ) ++ "]"
    | otherwise = (T.unpack one ) ++ "[" ++ (T.unpack two ) ++ "]." ++ (T.unpack three )