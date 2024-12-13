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

main :: IO()
-- main = do
--     args <- getArgs
--     case args of
--         [file, id1, id2] -> do
--             s <- readFile file
--             case (runImportParser "" (T.pack s)) of
--                 Left err -> print err
--                 Right (Imp {file = path}:impps) -> do
--                     f2 <- readFile (T.unpack path)
--                     case (runTestParser "" (T.pack f2)) of
--                         Left err -> print err
--                         Right lo_imp -> do
--                             case (runNetworkParser "" (T.pack s)) of 
--                                 Left err -> print err
--                                 Right parse -> 
--                                     let merged = mergeMaps (langDef (lang parse)) lo_imp in  
--                                     let (lo,tt) = runSpecification (Language {langDef = merged}) in
--                                         case runEvaluator parse (lo,tt) of
--                                             Left err -> print err
--                                             Right (binds, ts) -> 
--                                                 case TPL.performComputation (T.pack id1) (T.pack id2) tt ts of
--                                                     Left err -> print err
--                                                     Right res -> print res
--         _ -> die "Usage:\n\
--                 \ htpl file.htpl id1 id2"

main = do
    args <- getArgs
    case args of
        [file, id1, id2] -> do
            s <- readFile file
            case (runNetworkParser "" (T.pack s)) of 
                Left err -> --print err
                    do putStrLn err
                Right parse -> 
                    let (lo,tt) = runSpecification (lang parse) in
                        case runEvaluator parse (lo,tt) of
                            Left err -> print err
                            Right (binds, ts) -> 
                                case TPL.performComputation (T.pack id1) (T.pack id2) tt ts of
                                    Left err -> print err
                                    Right res -> --print res
                                        do putStrLn (printSuperPolicy res)
        _ -> die "Usage:\n\
                \ htpl file.htpl id1 id2"


-- handleImports :: [Import] -> Either (IO String) LanguageOptions
-- handleImports (Imp {file = path}:imps) = do
--     content <- readFile (T.unpack path)
--     case (runLOParser "" (T.pack content)) of
--         Left err -> return $ Left err
--         Right importLanguage -> do
--             case handleImports imps of
--                 Left err -> Left err
--                 Right langOption -> do
--                     let merged = mergeMaps langOption importLanguage in
--                         return merged

-- handleImports [] = do
--     return $ M.empty                     
-- Parser test
-- main = case (runNetworkParser "" (T.pack test4)) of
--     Left err -> print err
--     Right r -> print r

-- EvalTest
-- main =
--     let id1 = "manager" in 
--     let id2 = "manager" in
--     case (runNetworkParser "" (T.pack test3)) of 
--         Left err -> print err
--         Right parse -> --do
--             let (l,tt) = runSpecification (lang parse) in
--             case runEvaluator parse (l,tt) of
--                 Left err -> print err
--                 Right (binds, ts) -> 
--                     case TPL.performComputation (T.pack id1) (T.pack id2) tt ts of
--                         Left err -> print err
--                         Right res -> print res 




lan1 = "lang Access {gate, gate.open, gate.close, gate[front], gate[front].open, gate[front].close}."
evalDel = "group SecurityDepartment = [guard_1, guard_2, guard_3, manager]; trust (manager, SecurityDepartment) with {Access: gate[front]}."
test3 = lan1 ++ evalDel

mergeMaps :: LanguageOptions -> LanguageOptions -> LanguageOptions
mergeMaps m1 m2 = do
    -- keys <- M.keys m2
    mergeMaps' m1 m2 (M.keys m2)


mergeMaps' :: LanguageOptions -> LanguageOptions -> [ATag] -> LanguageOptions
mergeMaps' m1 m2 (key:keys) = do
    case M.lookup key m2 of
        Nothing -> undefined
        Just value2 -> do
            case M.lookup key m1 of
                Nothing -> do
                    mergeMaps' (M.insert key value2  m1) m2 keys
                Just value1 ->  do
                    mergeMaps' (M.insert key (nub $ value1++value2) m1) m2 keys
mergeMaps' m1 _m2 [] = m1



-- evalTest :: String -> String -> String -> IO()
-- evalTest network id1 id2 = do
--     case (runImportParser "" (T.pack network)) of
--         Left err -> print err
--         Right (Imp {file = path}:impps) -> do
--             f2 <- readFile (T.unpack path)
--             case (runTestParser "" (T.pack f2)) of
--                 Left err -> print err
--                 Right lo_imp -> do
--                     case (runNetworkParser "" (T.pack network)) of 
--                         Left err -> print err
--                         Right parse -> 
--                             let merged = mergeMaps (langDef (lang parse)) lo_imp in  
--                             let (lo,tt) = runSpecification (Language {langDef = merged}) in
--                                 case runEvaluator parse (lo,tt) of
--                                     Left err -> print err
--                                     Right (binds, ts) -> 
--                                         case TPL.performComputation (T.pack id1) (T.pack id2) tt ts of
--                                             Left err -> print err
--                                             Right res -> print res


test4 = "lang Access {ship, ship[bridge], ship[engine], ship[engine].fuzeBox}; \
\ lang Navigation {course, course.set, course.check};\
\lang Equipment {tools, tools[electric], tools[blowtourch]}.\
\\
\policy DeckAccess = {Access: ship[bridge], Navigation: Course};\
\policy EngineAccess  = {Access: ship[engine], Equipment: tools};\
\\
\group EngineDepartment = [chief_enginner, second_enginner, engine_cadet];\
\group DeckDepartment = [captain, chief_mate, second_mate];\
\\
\trust (captain, EngineDepartment) with EngineAccess;\
\trust (captain, DeckDepartment) with DeckAccess;\
\\
\if (engine_cadet in EngineDepartment) then {\
\    trust(captain, engine_cadet) with {Access: ship[engine]._}\
\} else {};\
\\
\if (eval(cheif_enginner, engine_cadet= {Equipment: tools[electric]})) then {\
\    if (eval(cheif_enginner, engine_cadet= {Equipment: tools[blowtourch]})) then {\
\        trust(captain, engine_cadet) with {Access: ship[engine]}\
\    } else {\
\        trust(captain, engine_cadet) with {Access: ship[engine].fuzeBox}\
\    }\
\} else {\
\    if (eval(cheif_enginner, engine_cadet= {Equipment: tools[blowtourch]})) then {\
\        trust(captain, engine_cadet) with {Access: ship[engine]}\
\    } else {}\
\}."


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