-- Blackbox testing of parser
{-# LANGUAGE OverloadedStrings        #-}

import AST
import Parser

import Test.Tasty
import Test.Tasty.HUnit
import qualified Data.Text as T
import qualified Data.Map.Strict as M

main :: IO ()
main = defaultMain $ localOption (mkTimeout 1000000) parserTests

-- tests :: TestTree
-- tests = parserTests ++ evaluatorTests

parserTests :: TestTree
parserTests = testGroup "Parser tests" [
    testCase "parse empty network" $ runNetworkParser "" "" @?=
        Right (Network {imp = [], lang = Language {langDef = M.fromList []}, exps = []}),

    testCase "Single import" $ runNetworkParser "" "import testfile.lan." @?= 
        Right (Network {imp = ["testfile.lan"], lang = Language {langDef = M.fromList []}, exps = []}),
 
    testCase "Two imports import" $ runNetworkParser "" "import testfile.lan, import testfile2.lan." @?= 
        Right (Network {imp = ["testfile.lan", "testfile2.lan"], lang = Language {langDef = M.fromList []}, exps = []}),
    
    testCase "Multiple imports import" $ runNetworkParser "" "import testfile.lan, import testfile2.lan, import testfile3.lan." @?= 
        Right (Network {imp = ["testfile.lan", "testfile2.lan", "testfile3.lan"], lang = Language {langDef = M.fromList []}, exps = []}),
    
    testCase "Bad import file" $ runNetworkParser "" "import badfile.lang." @?= 
        Left _,
    
    testCase "Simple language" $ runNetworkParser "" "lang Tag: {aspect1}" @?=
        Right (Network {imp = [], lang = Language {langDef = M.fromList [("Tag", ["aspect1"])]}, exps = []}),

    testCase "Multiple aspect tags" $ runNetworkParser "" "lang Tag1: {aspect1}; lang Tag2: {aspect2}" @?=
        Right (Network {imp = [], lang = Language {langDef = M.fromList [("Tag1", ["aspect1"]), ("Tag2", ["aspect2"])]}, exps = []}),
    "Simple language"
    "Multiple tags language"
    "Multiple options language"

    "Import and language"

    ""
    
    ]
-- evaluatorTests :: TestTree
-- evaluatorTests = testGroup "Evaluator tests" [
--     testCase 
--     ]
