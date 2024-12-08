-- Blackbox testing of parser
{-# LANGUAGE OverloadedStrings        #-}

import AST
import Parser

import Test.Tasty
import Test.Tasty.HUnit
import qualified Data.Text as T
import qualified Data.Map.Strict as M

main :: IO ()
main = defaultMain $ localOption (mkTimeout 1000000) tests

-- tests :: TestTree
tests = testGroup "Parser Tests" [parserTests, expressionTest]

parserTests :: TestTree
parserTests = testGroup "Imports and language" [
    testCase "Empty network" $ runNetworkParser "" "" @?=
        Right (Network {imp = [], lang = Language {langDef = M.fromList []}, exps = []}),

    testCase "Single import" $ runNetworkParser "" "import testfile.lan." @?= 
        Right (Network {imp = [Imp {file = "testfile"}], lang = Language {langDef = M.fromList []}, exps = []}),
 
    testCase "Wrong seperation" $ runNetworkParser "" "import testfile.lan, import testfile2.lan." @?= 
        Left "1:20:\n  |\n1 | import testfile.lan, import testfile2.lan.\n  |                    ^\nunexpected ','\nexpecting '.' or ';'\n",

    testCase "Two imports import" $ runNetworkParser "" "import testfile.lan; import testfile2.lan." @?= 
        Right (Network {imp = [Imp {file = "testfile"}, Imp {file = "testfile2"}], lang = Language {langDef = M.fromList []}, exps = []}),
    
    testCase "Multiple imports import" $ runNetworkParser "" "import testfile.lan; import testfile2.lan; import testfile3.lan." @?= 
        Right (Network {imp = [Imp {file = "testfile"}, Imp {file = "testfile2"}, Imp {file = "testfile3"}], lang = Language {langDef = M.fromList []}, exps = []}),
    
    testCase "Bad import file" $ runNetworkParser "" "import badfile.lang." @?= 
        Left "1:19:\n  |\n1 | import badfile.lang.\n  |                   ^\nunexpected 'g'\nexpecting '.' or ';'\n",
    
    testCase "Simple language" $ runNetworkParser "" "lang Tag {aspect1}." @?=
        Right (Network {imp = [], lang = Language {langDef = M.fromList [("Tag", [TDNS (Node (Atom "aspect1") (Leaf LTop) (Leaf LTop))])]}, exps = []}),

    testCase "Multiple aspect tags" $ runNetworkParser "" "lang Tag1 {aspect1}; lang Tag2 {aspect2}." @?=
        Right (Network {imp = [], lang = Language {langDef = M.fromList [("Tag1", [TDNS (Node (Atom "aspect1") (Leaf LTop) (Leaf LTop))]), ("Tag2", [TDNS (Node (Atom "aspect2") (Leaf LTop) (Leaf LTop))])]}, exps = []}),
    
    testCase "Multiple language options" $ runNetworkParser "" "lang Tag {aspect1, aspect2, aspect3}." @?=
        Right (Network {imp = [], lang = Language {langDef = M.fromList [("Tag", [TDNS (Node (Atom "aspect1") (Leaf LTop) (Leaf LTop)), TDNS (Node (Atom "aspect2") (Leaf LTop) (Leaf LTop)), TDNS (Node (Atom "aspect3") (Leaf LTop) (Leaf LTop))])]}, exps = []}),
    
    testCase "TDNS Expressions in language" $ runNetworkParser "" "lang Tag {aspect1, aspect1[leaf], aspect1.leaf2, aspect1[leaf1].leaf2}." @?=
        Right (Network {imp = [], lang = Language {langDef = M.fromList [("Tag",[TDNS (Node (Atom "aspect1") (Leaf LTop) (Leaf LTop)),TDNS (Node (Atom "aspect1") (Leaf (Atom "leaf")) (Leaf LTop)),TDNS (Node (Atom "aspect1") (Leaf LTop) (Leaf (Atom "leaf2"))),TDNS (Node (Atom "aspect1") (Leaf (Atom "leaf1")) (Leaf (Atom "leaf2")))])]}, exps = []}),
    
    testCase "Import and language" $ runNetworkParser "" "import testfile.lan. lang Tag {aspect1}." @?=
        Right (Network {imp = [Imp {file = "testfile"}], lang = Language {langDef = M.fromList [("Tag", [TDNS (Node (Atom "aspect1") (Leaf LTop) (Leaf LTop))])]}, exps = []})
    
    ]

expressionTest = testGroup "Expression tests" [
    testCase "Empty network" $ runNetworkParser "" "" @?=
        Right (Network {imp = [], lang = Language {langDef = M.fromList []}, exps = []})


    -- delegations 
    -- groups
    ]
