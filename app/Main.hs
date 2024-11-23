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

main :: IO()
-- main = case (parse language "" "test.test") of
main = case (runNetworkParser "" "import language.lan. when x in a do {Test.} otherwise {test.}.") of
    Left err -> print err
    Right r -> print r

