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
    testCase "Missing period." $ runNetworkParser "" "trust(id1, id2) with {Tag: aspect1}" @?=
        Left "1:36:\n  |\n1 | trust(id1, id2) with {Tag: aspect1}\n  |                                    ^\nunexpected end of input\nexpecting \"import\", \"lang\", '.', or ';'\n",

    testCase "Contintue after period" $ runNetworkParser "" "trust(id2, id3) with {Tag_: aspect2@_-$}. trust(id4,id6) with {Tag_: aspect1}" @?=
        Left "1:43:\n  |\n1 | trust(id2, id3) with {Tag_: aspect2@_-$}. trust(id4,id6) with {Tag_: aspect1}\n  |                                           ^\nunexpected 't'\nexpecting \"import\", \"lang\", '.', or end of input\n",

    testCase "Trust statement" $ runNetworkParser "" "trust(id2, id3) with {Tag: aspect2}." @?=
        Right (Network {imp = [], lang = Language {langDef =M.fromList []}, exps = [EDel "id2" "id3" (EPol (Policy  (M.fromList [("Tag",TDNS (Node (Atom "aspect2") (Leaf LTop) (Leaf LTop)))])))]}),

    testCase "Multiple trust statements" $ runNetworkParser "" "trust(id1, id2) with {Tag: aspect1}; trust(id3, id4) with {Tag2: aspect2}." @?=
        Right (Network {imp = [], lang = Language {langDef = M.fromList []}, exps = [EDel "id1" "id2" (EPol (Policy  (M.fromList [("Tag",TDNS (Node (Atom "aspect1") (Leaf LTop) (Leaf LTop)))]))),EDel "id3" "id4" (EPol (Policy  (M.fromList [("Tag2",TDNS (Node (Atom "aspect2") (Leaf LTop) (Leaf LTop)))])))]}),

    testCase "Wrong seperation" $ runNetworkParser "" "trust(id1, id2) with {Tag: aspect1}, trust(id1, id2) with {Tag: aspect1}." @?=
        Left "1:36:\n  |\n1 | trust(id1, id2) with {Tag: aspect1}, trust(id1, id2) with {Tag: aspect1}.\n  |                                    ^\nunexpected ','\nexpecting \"import\", \"lang\", '.', or ';'\n",

    testCase "Trust with Policy variable" $ runNetworkParser "" "trust(id1, id2) with Policy." @?=
        Right (Network {imp = [], lang = Language {langDef = M.fromList []}, exps = [EDel "id1" "id2" (EVar "Policy")]}),

    testCase "Group expression" $ runNetworkParser "" "group Name = [user]." @?=
        Right (Network {imp = [], lang = Language {langDef = M.fromList []}, exps = [EGroup "Name" ["user"]]}),

    testCase "Group expression: 2 users" $ runNetworkParser "" "group Name = [user, user2]." @?=
        Right (Network {imp = [], lang = Language {langDef = M.fromList []}, exps = [EGroup "Name" ["user","user2"]]}),

    testCase "Group expression: Whitespace" $ runNetworkParser "" "group Name = [user, user2,      user3]." @?=
        Right (Network {imp = [], lang = Language {langDef = M.fromList []}, exps = [EGroup "Name" ["user","user2","user3"]]}),

    testCase "Group expression: Whitespace 2" $ runNetworkParser "" "group Name = [user, user2,      user3,user4]." @?=
        Right (Network {imp = [], lang = Language {langDef = M.fromList []}, exps = [EGroup "Name" ["user","user2","user3","user4"]]}),

    testCase "Group expression: Whitespace 3" $ runNetworkParser "" "group      Name = [user, user2]." @?=
        Right (Network {imp = [], lang = Language {langDef = M.fromList []}, exps = [EGroup "Name" ["user","user2"]]}),

    testCase "Group expression: Whitespace 4" $ runNetworkParser "" "groupName = [user, user2]." @?=
        Right (Network {imp = [], lang = Language {langDef = M.fromList []}, exps = [EGroup "Name" ["user","user2"]]}),

    testCase "Group with Variable in list" $ runNetworkParser "" "group Name = [user, user2,     user3,user4, Group]." @?=
        Left "1:1:\n  |\n1 | group Name = [user, user2,     user3,user4, Group].\n  | ^\nunexpected 'g'\nexpecting \"import\", \"lang\", '.', or end of input\n",

    testCase "Group: Wrong seperation" $ runNetworkParser "" "group name = [user, user2; user3]." @?=
        Left "1:1:\n  |\n1 | group name = [user, user2; user3].\n  | ^\nunexpected 'g'\nexpecting \"import\", \"lang\", '.', or end of input\n",

    testCase "Predicate" $ runNetworkParser "" "group Name = pred X in {a, X: {Tag: test}}." @?=
        Right (Network {imp = [], lang = Language {langDef = M.fromList []}, exps = [EPred "Name" "X" [Pred "a" "X" (EPol (Policy  (M.fromList [("Tag",TDNS (Node (Atom "test") (Leaf LTop) (Leaf LTop)))])))]]}),

    testCase "Predicate 2: Bigger policy" $ runNetworkParser "" "group Name = pred X in {a, X: {Tag: test, Tag2: test2}}." @?=
        Right (Network {imp = [], lang = Language {langDef = M.fromList []}, exps = [EPred "Name" "X" [Pred "a" "X" (EPol (Policy  (M.fromList [("Tag",TDNS (Node (Atom "test") (Leaf LTop) (Leaf LTop))),("Tag2",TDNS (Node (Atom "test2") (Leaf LTop) (Leaf LTop)))])))]]}),

    testCase "Predicate 3: Multiple predicates" $ runNetworkParser "" "group Name = pred X in {a, X: {Tag: test, Tag2: test2}; X, a: {Tag: test}}." @?=
        Right (Network {imp = [], lang = Language {langDef = M.fromList []}, exps = [EPred "Name" "X" [Pred "a" "X" (EPol (Policy  (M.fromList [("Tag",TDNS (Node (Atom "test") (Leaf LTop) (Leaf LTop))),("Tag2",TDNS (Node (Atom "test2") (Leaf LTop) (Leaf LTop)))]))),Pred "X" "a" (EPol (Policy  (M.fromList [("Tag",TDNS (Node (Atom "test") (Leaf LTop) (Leaf LTop)))])))]]})
       
    
    
    -- "Whitespace in trust" $ runNetworkParser "" "" @?=


    -- testCase "" $ runNetworkParser "" "" @?=



    -- delegations 
    -- groups
    -- $ runNetworkParser "" "" @?=
    ]

-- literalsTests = testGroup "Literals" [

--     "Group tag"

--     "Speciel character aspect"

--     "Variable"

--     "Tag with underscore"

--     "Tag with numbers"
--     ]