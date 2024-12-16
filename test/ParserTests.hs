-- Blackbox testing of parser
{-# LANGUAGE OverloadedStrings #-}

import AST
import qualified Data.Map.Strict as M
import qualified Data.Text  as T
import Parser
import RunEvaluator
import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main = defaultMain $ localOption (mkTimeout 1000000) tests

-- tests :: TestTree
tests = testGroup "HTPL - Parser Testing:" [parserTests, expressionTest, policyTempTest, implicationTests, whenTests, ifTests, literalsTests, delegationTests]

parserTests :: TestTree
parserTests =
  testGroup
    "Imports and language"
    [ testCase "Empty network" $
        runNetworkParser "" ""
          @?= Right (Network {imp = [], lang = Language {langDef = M.fromList []}, exps = []}),
      testCase "Single import" $
        runNetworkParser "" "import testfile.lan."
          @?= Right (Network {imp = [Imp {file = "testfile.lan"}], lang = Language {langDef = M.fromList []}, exps = []}),
      testCase "Wrong seperation" $
        runNetworkParser "" "import testfile.lan, import testfile2.lan."
          @?= Left "1:20:\n  |\n1 | import testfile.lan, import testfile2.lan.\n  |                    ^\nunexpected ','\nexpecting '.' or ';'\n",
      testCase "Two imports import" $
        runNetworkParser "" "import testfile.lan; import testfile2.lan."
          @?= Right (Network {imp = [Imp {file = "testfile.lan"}, Imp {file = "testfile2.lan"}], lang = Language {langDef = M.fromList []}, exps = []}),
      testCase "Multiple imports import" $
        runNetworkParser "" "import testfile.lan; import testfile2.lan; import testfile3.lan."
          @?= Right (Network {imp = [Imp {file = "testfile.lan"}, Imp {file = "testfile2.lan"}, Imp {file = "testfile3.lan"}], lang = Language {langDef = M.fromList []}, exps = []}),
      testCase "Bad import file" $
        runNetworkParser "" "import badfile.lang."
          @?= Left "1:19:\n  |\n1 | import badfile.lang.\n  |                   ^\nunexpected 'g'\nexpecting '.' or ';'\n",
      testCase "Simple language" $
        runNetworkParser "" "lang Tag {aspect1}."
          @?= Right (Network {imp = [], lang = Language {langDef = M.fromList [("Tag", [TDNS (Node (Atom "aspect1") (Leaf LTop) (Leaf LTop))])]}, exps = []}),
      testCase "Multiple aspect tags" $
        runNetworkParser "" "lang Tag1 {aspect1}; lang Tag2 {aspect2}."
          @?= Right (Network {imp = [], lang = Language {langDef = M.fromList [("Tag1", [TDNS (Node (Atom "aspect1") (Leaf LTop) (Leaf LTop))]), ("Tag2", [TDNS (Node (Atom "aspect2") (Leaf LTop) (Leaf LTop))])]}, exps = []}),
      testCase "Multiple language options" $
        runNetworkParser "" "lang Tag {aspect1, aspect2, aspect3}."
          @?= Right (Network {imp = [], lang = Language {langDef = M.fromList [("Tag", [TDNS (Node (Atom "aspect1") (Leaf LTop) (Leaf LTop)), TDNS (Node (Atom "aspect2") (Leaf LTop) (Leaf LTop)), TDNS (Node (Atom "aspect3") (Leaf LTop) (Leaf LTop))])]}, exps = []}),
      testCase "TDNS Expressions in language" $
        runNetworkParser "" "lang Tag {aspect1, aspect1[leaf], aspect1.leaf2, aspect1[leaf1].leaf2}."
          @?= Right (Network {imp = [], lang = Language {langDef = M.fromList [("Tag", [TDNS (Node (Atom "aspect1") (Leaf LTop) (Leaf LTop)), TDNS (Node (Atom "aspect1") (Leaf (Atom "leaf")) (Leaf LTop)), TDNS (Node (Atom "aspect1") (Leaf LTop) (Leaf (Atom "leaf2"))), TDNS (Node (Atom "aspect1") (Leaf (Atom "leaf1")) (Leaf (Atom "leaf2")))])]}, exps = []}),
      testCase "Import and language" $
        runNetworkParser "" "import testfile.lan. lang Tag {aspect1}."
          @?= Right (Network {imp = [Imp {file = "testfile.lan"}], lang = Language {langDef = M.fromList [("Tag", [TDNS (Node (Atom "aspect1") (Leaf LTop) (Leaf LTop))])]}, exps = []})
    ]

expressionTest =
  testGroup
    "Expression tests"
    [ testCase "Missing period." $
        runNetworkParser "" "trust(id1, id2) with {Tag: aspect1}"
          @?= Left "1:36:\n  |\n1 | trust(id1, id2) with {Tag: aspect1}\n  |                                    ^\nunexpected end of input\nexpecting \"import\", \"lang\", '.', or ';'\n",
      testCase "Contintue after period" $
        runNetworkParser "" "trust(id2, id3) with {Tag_: aspect2@_-$}. trust(id4,id6) with {Tag_: aspect1}"
          @?= Left "1:43:\n  |\n1 | trust(id2, id3) with {Tag_: aspect2@_-$}. trust(id4,id6) with {Tag_: aspect1}\n  |                                           ^\nunexpected 't'\nexpecting \"import\", \"lang\", '.', or end of input\n",
      testCase "Trust statement" $
        runNetworkParser "" "trust(id2, id3) with {Tag: aspect2}."
          @?= Right (Network {imp = [], lang = Language {langDef = M.fromList []}, exps = [EDel "id2" "id3" (EPol (Policy (M.fromList [("Tag", TDNS (Node (Atom "aspect2") (Leaf LTop) (Leaf LTop)))])))]}),
      testCase "Multiple trust statements" $
        runNetworkParser "" "trust(id1, id2) with {Tag: aspect1}; trust(id3, id4) with {Tag2: aspect2}."
          @?= Right (Network {imp = [], lang = Language {langDef = M.fromList []}, exps = [EDel "id1" "id2" (EPol (Policy (M.fromList [("Tag", TDNS (Node (Atom "aspect1") (Leaf LTop) (Leaf LTop)))]))), EDel "id3" "id4" (EPol (Policy (M.fromList [("Tag2", TDNS (Node (Atom "aspect2") (Leaf LTop) (Leaf LTop)))])))]}),
      testCase "Wrong seperation" $
        runNetworkParser "" "trust(id1, id2) with {Tag: aspect1}, trust(id1, id2) with {Tag: aspect1}."
          @?= Left "1:36:\n  |\n1 | trust(id1, id2) with {Tag: aspect1}, trust(id1, id2) with {Tag: aspect1}.\n  |                                    ^\nunexpected ','\nexpecting \"import\", \"lang\", '.', or ';'\n",
      testCase "Trust with Policy variable" $
        runNetworkParser "" "trust(id1, id2) with Policy."
          @?= Right (Network {imp = [], lang = Language {langDef = M.fromList []}, exps = [EDel "id1" "id2" (EVar "Policy")]}),
      testCase "Group expression" $
        runNetworkParser "" "group Name = [user]."
          @?= Right (Network {imp = [], lang = Language {langDef = M.fromList []}, exps = [EGroup "Name" ["user"]]}),
      testCase "Group expression: 2 users" $
        runNetworkParser "" "group Name = [user, user2]."
          @?= Right (Network {imp = [], lang = Language {langDef = M.fromList []}, exps = [EGroup "Name" ["user", "user2"]]}),
      testCase "Group expression: Whitespace" $
        runNetworkParser "" "group Name = [user, user2,      user3]."
          @?= Right (Network {imp = [], lang = Language {langDef = M.fromList []}, exps = [EGroup "Name" ["user", "user2", "user3"]]}),
      testCase "Group expression: Whitespace 2" $
        runNetworkParser "" "group Name = [user, user2,      user3,user4]."
          @?= Right (Network {imp = [], lang = Language {langDef = M.fromList []}, exps = [EGroup "Name" ["user", "user2", "user3", "user4"]]}),
      testCase "Group expression: Whitespace 3" $
        runNetworkParser "" "group      Name = [user, user2]."
          @?= Right (Network {imp = [], lang = Language {langDef = M.fromList []}, exps = [EGroup "Name" ["user", "user2"]]}),
      testCase "Group expression: Whitespace 4" $
        runNetworkParser "" "groupName = [user, user2]."
          @?= Right (Network {imp = [], lang = Language {langDef = M.fromList []}, exps = [EGroup "Name" ["user", "user2"]]}),
      testCase "Group with Variable in list" $
        runNetworkParser "" "group Name = [user, user2,     user3,user4, Group]."
          @?= Left "1:45:\n  |\n1 | group Name = [user, user2,     user3,user4, Group].\n  |                                             ^\nUser name must begin with lower case character and can not contain special symbols.\n",
      testCase "Group: Wrong seperation" $
        runNetworkParser "" "group Name = [user, user2; user3]."
          @?= Left "1:26:\n  |\n1 | group Name = [user, user2; user3].\n  |                          ^\nunexpected ';'\nexpecting \"import\", \"lang\", ',', '.', ']', '_', or alphanumeric character\n",
      testCase "Group: Bad variable name" $
        runNetworkParser "" "group name = [user, user2; user3]."
          @?= Left "1:7:\n  |\n1 | group name = [user, user2; user3].\n  |       ^\nVariable name not properly formed.\n",
      testCase "Predicate 1: Simple predicate" $
        runNetworkParser "" "group Name = pred X in {a, X: {Tag: test}}."
          @?= Right (Network {imp = [], lang = Language {langDef = M.fromList []}, exps = [EPred "Name" "X" [Pred "a" "X" (EPol (Policy (M.fromList [("Tag", TDNS (Node (Atom "test") (Leaf LTop) (Leaf LTop)))])))]]}),
      testCase "Predicate 2: Bigger policy" $
        runNetworkParser "" "group Name = pred X in {a, X: {Tag: test, Tag2: test2}}."
          @?= Right (Network {imp = [], lang = Language {langDef = M.fromList []}, exps = [EPred "Name" "X" [Pred "a" "X" (EPol (Policy (M.fromList [("Tag", TDNS (Node (Atom "test") (Leaf LTop) (Leaf LTop))), ("Tag2", TDNS (Node (Atom "test2") (Leaf LTop) (Leaf LTop)))])))]]}),
      testCase "Predicate 3: Multiple predicates" $
        runNetworkParser "" "group Name = pred X in {a, X: {Tag: test, Tag2: test2}; X, a: {Tag: test}}."
          @?= Right (Network {imp = [], lang = Language {langDef = M.fromList []}, exps = [EPred "Name" "X" [Pred "a" "X" (EPol (Policy (M.fromList [("Tag", TDNS (Node (Atom "test") (Leaf LTop) (Leaf LTop))), ("Tag2", TDNS (Node (Atom "test2") (Leaf LTop) (Leaf LTop)))]))), Pred "X" "a" (EPol (Policy (M.fromList [("Tag", TDNS (Node (Atom "test") (Leaf LTop) (Leaf LTop)))])))]]})
    ]

policyTempTest =
  testGroup
    "Policy template tests:"
    [ testCase "Policy template" $
        runNetworkParser "" "policy Test = {Tag: aspect}."
          @?= Right (Network {imp = [], lang = Language {langDef = M.fromList []}, exps = [EPolTmp "Test" (EPol (Policy (M.fromList [("Tag", TDNS (Node (Atom "aspect") (Leaf LTop) (Leaf LTop)))])))]}),
      testCase "Policy template with policy variable" $
        runNetworkParser "" "policy Test = Policy."
          @?= Right (Network {imp = [], lang = Language {langDef = M.fromList []}, exps = [EPolTmp "Test" (EVar "Policy")]}),
      testCase "Policy template - Whitespace" $
        runNetworkParser "" "policy Test           =   Policy."
          @?= Right (Network {imp = [], lang = Language {langDef = M.fromList []}, exps = [EPolTmp "Test" (EVar "Policy")]}),
      testCase "Policy template - Wrong symbol" $
        runNetworkParser "" "policy                Test : Policy."
          @?= Left "1:28:\n  |\n1 | policy                Test : Policy.\n  |                            ^\nunexpected ':'\nexpecting \"import\", \"lang\", '.', or '='\n"
    ]

implicationTests =
  testGroup
    "Implication tests:"
    [ testCase "For expression" $
        runNetworkParser "" "for X where {X, paul: Policy} do {trust(X,paul) with Policy}."
          @?= Right (Network {imp = [], lang = Language {langDef = M.fromList []}, exps = [EImp "X" [Pred "X" "paul" (EVar "Policy")] [EDel "X" "paul" (EVar "Policy")]]}),
      testCase "For expression - Empty predicate" $
        runNetworkParser "" "for X where {} do {}."
          @?= Left "1:14:\n  |\n1 | for X where {} do {}.\n  |              ^\nAtom ill-formed.\nUser name must begin with lower case character and can not contain special symbols.\nVariable name not properly formed.\n",
      testCase "For expression - Empty expression" $
        runNetworkParser "" "for X where {X,paul: Policy} do {}."
          @?= Right (Network {imp = [], lang = Language {langDef = M.fromList []}, exps = [EImp "X" [Pred "X" "paul" (EVar "Policy")] []]}),
      testCase "For expression - Nested for" $
        runNetworkParser "" "for X where {X,paul: Policy} do {for X where {X,paul: Policy} do {}}."
          @?= Right (Network {imp = [], lang = Language {langDef = M.fromList []}, exps = [EImp "X" [Pred "X" "paul" (EVar "Policy")] [EImp "X" [Pred "X" "paul" (EVar "Policy")] []]]}),
      testCase "For expression - Nested for 5 times" $
        runNetworkParser "" "for X where {X,paul: Policy} do {for X where {X,paul: Policy} do {for X where {X,paul: Policy} do {for X where {X,paul: Policy} do {for X where {X,paul: Policy} do {}}}}}."
          @?= Right (Network {imp = [], lang = Language {langDef = M.fromList []}, exps = [EImp "X" [Pred "X" "paul" (EVar "Policy")] [EImp "X" [Pred "X" "paul" (EVar "Policy")] [EImp "X" [Pred "X" "paul" (EVar "Policy")] [EImp "X" [Pred "X" "paul" (EVar "Policy")] [EImp "X" [Pred "X" "paul" (EVar "Policy")] []]]]]]})
    ]

whenTests =
  testGroup
    "When tests:"
    [ testCase "When expression" $
        runNetworkParser "" "when (x in Xs) do {trust(id1, id2) with Policy} otherwise {trust(id1, id2) with Policy}."
          @?= Right (Network {imp = [], lang = Language {langDef = M.fromList []}, exps = [EWhen (RIn "x" "Xs") [EDel "id1" "id2" (EVar "Policy")] [EDel "id1" "id2" (EVar "Policy")]]}),
      testCase "When expression - empty relation" $
        runNetworkParser "" "when () do {trust(id1, id2) with Policy} otherwise {trust(id1, id2) with Policy}."
          @?= Left "1:7:\n  |\n1 | when () do {trust(id1, id2) with Policy} otherwise {trust(id1, id2) with Policy}.\n  |       ^\nRelation not properly formattet.\nUser name must begin with lower case character and can not contain special symbols.\nVariable name not properly formed.\n",
      testCase "When expression - Eval relation" $
        runNetworkParser "" "when (eval(id1, id2 = Policy)) do {trust(id1, id2) with Policy} otherwise {trust(id1, id2) with Policy}."
          @?= Right (Network {imp = [], lang = Language {langDef = M.fromList []}, exps = [EWhen (REval "id1" "id2" (EVar "Policy")) [EDel "id1" "id2" (EVar "Policy")] [EDel "id1" "id2" (EVar "Policy")]]}),
      testCase "When expression - Size relation Less" $
        runNetworkParser "" "when (Group < 10) do {trust(id1, id2) with Policy} otherwise {trust(id1, id2) with Policy}."
          @?= Right (Network {imp = [], lang = Language {langDef = M.fromList []}, exps = [EWhen (RSize "Group" Less 10) [EDel "id1" "id2" (EVar "Policy")] [EDel "id1" "id2" (EVar "Policy")]]}),
      testCase "When expression - Size relation Equal" $
        runNetworkParser "" "when (Group == 10) do {trust(id1, id2) with Policy} otherwise {trust(id1, id2) with Policy}."
          @?= Right (Network {imp = [], lang = Language {langDef = M.fromList []}, exps = [EWhen (RSize "Group" Eq 10) [EDel "id1" "id2" (EVar "Policy")] [EDel "id1" "id2" (EVar "Policy")]]}),
      testCase "When expression - Size relation Greater" $
        runNetworkParser "" "when (Group > 10) do {trust(id1, id2) with Policy} otherwise {trust(id1, id2) with Policy}."
          @?= Right (Network {imp = [], lang = Language {langDef = M.fromList []}, exps = [EWhen (RSize "Group" Greater 10) [EDel "id1" "id2" (EVar "Policy")] [EDel "id1" "id2" (EVar "Policy")]]}),
      testCase "When expression - In relation" $
        runNetworkParser "" "when (   x   in    Xs   ) do {trust(id1, id2) with Policy} otherwise {trust(id1, id2) with Policy}."
          @?= Right (Network {imp = [], lang = Language {langDef = M.fromList []}, exps = [EWhen (RIn "x" "Xs") [EDel "id1" "id2" (EVar "Policy")] [EDel "id1" "id2" (EVar "Policy")]]}),
      testCase "When expression - Not In relation" $
        runNetworkParser "" "when (not x in Xs) do {trust(id1, id2) with Policy} otherwise {trust(id1, id2) with Policy}."
          @?= Right (Network {imp = [], lang = Language {langDef = M.fromList []}, exps = [EWhen (RNot (RIn "x" "Xs")) [EDel "id1" "id2" (EVar "Policy")] [EDel "id1" "id2" (EVar "Policy")]]})
    ]

ifTests =
  testGroup
    "If tests:"
    [ testCase "If expression" $
        runNetworkParser "" "if ( not x in Xs ) then {trust(x1, x2) with Policy1} else{trust (x2,x1) with Policy2}."
          @?= Right (Network {imp = [], lang = Language {langDef = M.fromList []}, exps = [EIf (RNot (RIn "x" "Xs")) [EDel "x1" "x2" (EVar "Policy1")] [EDel "x2" "x1" (EVar "Policy2")]]}),
      testCase "If expression - Empty relation" $
        runNetworkParser "" "if (  ) then {trust(x1, x2) with Policy1} else{trust (x2,x1) with Policy2}."
          @?= Left "1:7:\n  |\n1 | if (  ) then {trust(x1, x2) with Policy1} else{trust (x2,x1) with Policy2}.\n  |       ^\nRelation not properly formattet.\nUser name must begin with lower case character and can not contain special symbols.\nVariable name not properly formed.\n",
      testCase "If expression - Nested if" $
        runNetworkParser "" "if ( x in Xs ) then {if (x1 in Xs) then  {trust (x1,x2) with Policy3} else {}} else{trust (x2,x1) with Policy2}."
          @?= Right (Network {imp = [], lang = Language {langDef = M.fromList []}, exps = [EIf (RIn "x" "Xs") [EIf (RIn "x1" "Xs") [EDel "x1" "x2" (EVar "Policy3")] []] [EDel "x2" "x1" (EVar "Policy2")]]}),
      testCase "If expression - Trust Group -> User" $
        runNetworkParser "" "if ( x in Xs ) then {trust(x, Xs) with Policy1} else{trust (x2,x1) with Policy2}."
          @?= Right (Network {imp = [], lang = Language {langDef = M.fromList []}, exps = [EIf (RIn "x" "Xs") [EDel "x" "Xs" (EVar "Policy1")] [EDel "x2" "x1" (EVar "Policy2")]]}),
      testCase "If expression - Trust User -> Group" $
        runNetworkParser "" "if ( x in Xs ) then {trust(Xs, x) with Policy1} else{trust (x2,x1) with Policy2}."
          @?= Right (Network {imp = [], lang = Language {langDef = M.fromList []}, exps = [EIf (RIn "x" "Xs") [EDel "Xs" "x" (EVar "Policy1")] [EDel "x2" "x1" (EVar "Policy2")]]})
    ]

literalsTests =
  testGroup
    "Literals"
    [ testCase "Aspect tag 1" $
        runNetworkParser "" "lang Tag {aspect1}."
          @?= Right (Network {imp = [], lang = Language {langDef = M.fromList [("Tag", [TDNS (Node (Atom "aspect1") (Leaf LTop) (Leaf LTop))])]}, exps = []}),
      testCase "Aspect tag 2" $
        runNetworkParser "" "lang Tag_ {aspect1}."
          @?= Right (Network {imp = [], lang = Language {langDef = M.fromList [("Tag_", [TDNS (Node (Atom "aspect1") (Leaf LTop) (Leaf LTop))])]}, exps = []}),
      testCase "Aspect tag 3" $
        runNetworkParser "" "lang Tag_test_12 {aspect1}."
          @?= Right (Network {imp = [], lang = Language {langDef = M.fromList [("Tag_test_12", [TDNS (Node (Atom "aspect1") (Leaf LTop) (Leaf LTop))])]}, exps = []}),
      testCase "Language expression 1" $
        runNetworkParser "" "lang Tag {Aspect1}."
          @?= Right (Network {imp = [], lang = Language {langDef = M.fromList [("Tag", [TDNS (Node (Atom "Aspect1") (Leaf LTop) (Leaf LTop))])]}, exps = []}),
      testCase "Language expression 2" $
        runNetworkParser "" "lang Tag {Aspe_ct1}."
          @?= Right (Network {imp = [], lang = Language {langDef = M.fromList [("Tag", [TDNS (Node (Atom "Aspe_ct1") (Leaf LTop) (Leaf LTop))])]}, exps = []}),
      testCase "Language expression 3" $
        runNetworkParser "" "lang Tag {Aspe-ct1}."
          @?= Right (Network {imp = [], lang = Language {langDef = M.fromList [("Tag", [TDNS (Node (Atom "Aspe-ct1") (Leaf LTop) (Leaf LTop))])]}, exps = []}),
      testCase "Language expression 4" $
        runNetworkParser "" "lang Tag {Aspe@ct1}."
          @?= Right (Network {imp = [], lang = Language {langDef = M.fromList [("Tag", [TDNS (Node (Atom "Aspe@ct1") (Leaf LTop) (Leaf LTop))])]}, exps = []}),
      testCase "Language expression 5" $
        runNetworkParser "" "lang Tag {Aspe$ct1}."
          @?= Right (Network {imp = [], lang = Language {langDef = M.fromList [("Tag", [TDNS (Node (Atom "Aspe$ct1") (Leaf LTop) (Leaf LTop))])]}, exps = []}),
      testCase "Language expression 6" $
        runNetworkParser "" "lang Tag {12aspe$ct1}."
          @?= Left "1:11:\n  |\n1 | lang Tag {12aspe$ct1}.\n  |           ^\nLanguage expression not properly formed.\nLanguage expression not well formed.\n",
      testCase "Variable 1" $
        runNetworkParser "" "group Var = [one]."
          @?= Right (Network {imp = [], lang = Language {langDef = M.fromList []}, exps = [EGroup "Var" ["one"]]}),
      testCase "Variable 2" $
        runNetworkParser "" "group Var120 = [one]."
          @?= Right (Network {imp = [], lang = Language {langDef = M.fromList []}, exps = [EGroup "Var120" ["one"]]}),
      testCase "Variable 3" $
        runNetworkParser "" "group var = [one]."
          @?= Left "1:7:\n  |\n1 | group var = [one].\n  |       ^\nVariable name not properly formed.\n",
      testCase "Filepath 1" $
        runNetworkParser "" "import file.lan."
          @?= Right (Network {imp = [Imp {file = "file.lan"}], lang = Language {langDef = M.fromList []}, exps = []}),
      testCase "Filepath 2" $
        runNetworkParser "" "import app/file.lan."
          @?= Right (Network {imp = [Imp {file = "app/file.lan"}], lang = Language {langDef = M.fromList []}, exps = []}),
      testCase "Filepath 3" $
        runNetworkParser "" "import ./app/file.lan."
          @?= Right (Network {imp = [Imp {file = "./app/file.lan"}], lang = Language {langDef = M.fromList []}, exps = []}),
      testCase "Filepath 4" $
        runNetworkParser "" "import ~/app/file.lan."
          @?= Right (Network {imp = [Imp {file = "~/app/file.lan"}], lang = Language {langDef = M.fromList []}, exps = []}),
      testCase "Keyword" $
        runNetworkParser "" "group Name = [group]."
          @?= Left "1:20:\n  |\n1 | group Name = [group].\n  |                    ^\nAtom can not be a reserved keyword!\n"
    ]

lang_1 = "lang Access {gate, gate.open, gate.close, gate[front], gate[front].open, gate[front].close, _}."
evalDel = "group SecurityDepartment = [guard_1, guard_2, guard_3]; trust (manager, SecurityDepartment) with {Access: gate[front]}."
evalDel2 = "group SecurityDepartment = [guard_1, guard_2, guard_3]; trust (SecurityDepartment, manager) with {Access: gate[front]}."
policyTemplate = "policy Pol = {Access: gate[front]}; trust(id1, id2) with Pol."
revocation = "trust(id1, id2) with {Access: gate}; trust(id1, id2) with {Access: _}."

ifRelationEvalTrue = "trust(id1, id2) with {Access: gate}; if (eval(id1, id2 = {Access: gate})) then {trust(id4, id5) with {Access: gate.open}} else {trust(id4, id5) with {Access: gate.close}}."
ifRelationEvalFalse = "trust(id1, id2) with {Access: gate[front].open}; if (eval(id1, id2 = {Access: gate})) then {trust(id4, id5) with {Access: gate.open}} else {trust(id4, id5) with {Access: gate.close}}."
ifRelationEvalTrueSmallpolicy = "trust(id1, id2) with {Access: gate[front]}; if (eval(id1, id2 = {Access: gate[front].open})) then {trust(id4, id5) with {Access: gate.open}} else {trust(id4, id5) with {Access: gate.close}}."

ifRelationInTrue = "group SecurityDepartment = [guard_1, guard_2, guard_3]; if (guard_1 in SecurityDepartment) then {trust(id4, id5) with {Access: gate.open}} else {trust(id4, id5) with {Access: gate.close}}."
ifRelationInFalse = "group SecurityDepartment = [guard_1, guard_2, guard_3]; if (guard_4 in SecurityDepartment) then {trust(id4, id5) with {Access: gate.open}} else {trust(id4, id5) with {Access: gate.close}}."

ifRelationSizeTrueLess = "group SecurityDepartment = [guard_1, guard_2, guard_3]; if (SecurityDepartment < 4) then {trust(id4, id5) with {Access: gate.open}} else {trust(id4, id5) with {Access: gate.close}}."
ifRelationSizeTrueGreater = "group SecurityDepartment = [guard_1, guard_2, guard_3]; if (SecurityDepartment > 2) then {trust(id4, id5) with {Access: gate.open}} else {trust(id4, id5) with {Access: gate.close}}."
ifRelationSizeTrueEqual = "group SecurityDepartment = [guard_1, guard_2, guard_3]; if (SecurityDepartment == 3) then {trust(id4, id5) with {Access: gate.open}} else {trust(id4, id5) with {Access: gate.close}}."
ifRelationSizeFalseEqual = "group SecurityDepartment = [guard_1, guard_2, guard_3]; if (SecurityDepartment == 1000) then {trust(id4, id5) with {Access: gate.open}} else {trust(id4, id5) with {Access: gate.close}}."
ifRelationSizeFalseLess = "group SecurityDepartment = [guard_1, guard_2, guard_3]; if (SecurityDepartment < 0) then {trust(id4, id5) with {Access: gate.open}} else {trust(id4, id5) with {Access: gate.close}}."
ifRelationSizeFalseGreater = "group SecurityDepartment = [guard_1, guard_2, guard_3]; if (SecurityDepartment > 6) then {trust(id4, id5) with {Access: gate.open}} else {trust(id4, id5) with {Access: gate.close}}."

ifRelationNotEvalTrue = "trust(id1, id2) with {Access: gate.open}; if (not eval(id1, id2 = {Access: gate})) then {trust(id4, id5) with {Access: gate.open}} else {trust(id4, id5) with {Access: gate.close}}."
ifRelationNotEvalFalse = "trust(id1, id2) with {Access: gate}; if (not eval(id1, id2 = {Access: gate})) then {trust(id4, id5) with {Access: gate.open}} else {trust(id4, id5) with {Access: gate.close}}."

ifRelationNotInTrue = "group SecurityDepartment = [guard_1, guard_2, guard_3]; if (not guard_4 in SecurityDepartment) then {trust(id4, id5) with {Access: gate.open}} else {trust(id4, id5) with {Access: gate.close}}."
ifRelationNotInFalse = "group SecurityDepartment = [guard_1, guard_2, guard_3]; if (not guard_1 in SecurityDepartment) then {trust(id4, id5) with {Access: gate.open}} else {trust(id4, id5) with {Access: gate.close}}."

ifRelationNotSizeTrue = "group SecurityDepartment = [guard_1, guard_2, guard_3]; if (not SecurityDepartment > 4) then {trust(id4, id5) with {Access: gate.open}} else {trust(id4, id5) with {Access: gate.close}}."
ifRelationNotSizeFalse = "group SecurityDepartment = [guard_1, guard_2, guard_3]; if (not SecurityDepartment < 4) then {trust(id4, id5) with {Access: gate.open}} else {trust(id4, id5) with {Access: gate.close}}."

predicate = "trust(id1, id2) with {Access: gate.open}; trust(id1, id4) with {Access: gate.close}; group Test = pred X in { id1, X: {Access: gate}}; trust (id5, Test) with {Access: gate.open}."

delegationTests :: TestTree
delegationTests =
  testGroup
    "Evaluation: Delegation:"
    [ testCase "Empty network" $
        evalTest "" "id1" "id2"
          @?= Right (SuperPolicy {policies = [PBot]}),
      testCase "Delegation: Entity -> Entity" $
        evalTest (lang_1 ++ "trust(id1, id2) with {Access: gate.open}.") "id1" "id2"
          @?= Right (SuperPolicy {policies = [Policy (M.fromList [("Access", TDNS (Node (Atom "gate") (Leaf LTop) (Leaf (Atom "open"))))])]}),
      testCase "Delegation: Entity -> Group" $
        evalTest (lang_1 ++ evalDel) "manager" "guard_3"
          @?= Right (SuperPolicy {policies = [Policy (M.fromList [("Access", TDNS (Node (Atom "gate") (Leaf (Atom "front")) (Leaf LTop)))])]}),
      testCase "Delegation: Group -> Entity" $
        evalTest (lang_1 ++ evalDel2) "guard_3" "manager"
          @?= Right (SuperPolicy {policies = [Policy (M.fromList [("Access", TDNS (Node (Atom "gate") (Leaf (Atom "front")) (Leaf LTop)))])]}),
      testCase "Policy template" $
        evalTest (lang_1 ++ policyTemplate) "id1" "id2"
          @?= Right (SuperPolicy {policies = [Policy (M.fromList [("Access", TDNS (Node (Atom "gate") (Leaf (Atom "front")) (Leaf LTop)))])]}),
      testCase "Trust revocation" $
        evalTest (lang_1 ++ revocation) "id1" "id2"
          @?= Right (SuperPolicy {policies = [Policy (M.fromList [("Access", TDNS (Node LBot (Leaf LBot) (Leaf LBot)))])]}),
      testCase "If: Eval relation true" $
        evalTest (lang_1 ++ ifRelationEvalTrue) "id4" "id5"
          @?= Right (SuperPolicy {policies = [Policy (M.fromList [("Access", TDNS (Node (Atom "gate") (Leaf LTop) (Leaf (Atom "open"))))])]}),
      testCase "If: Eval relation false" $
        evalTest (lang_1 ++ ifRelationEvalFalse) "id4" "id5"
          @?= Right (SuperPolicy {policies = [Policy (M.fromList [("Access", TDNS (Node (Atom "gate") (Leaf LTop) (Leaf (Atom "close"))))])]}),
      testCase "If: Eval relation. Smaller policy" $
        evalTest (lang_1 ++ ifRelationEvalTrueSmallpolicy) "id4" "id5"
          @?= Right (SuperPolicy {policies = [Policy (M.fromList [("Access", TDNS (Node (Atom "gate") (Leaf LTop) (Leaf (Atom "open"))))])]}),
      testCase "If: In relation - true" $
        evalTest (lang_1 ++ ifRelationInTrue) "id4" "id5"
          @?= Right (SuperPolicy {policies = [Policy (M.fromList [("Access", TDNS (Node (Atom "gate") (Leaf LTop) (Leaf (Atom "open"))))])]}),
      testCase "If: In relation - false" $
        evalTest (lang_1 ++ ifRelationInFalse) "id4" "id5"
          @?= Right (SuperPolicy {policies = [Policy (M.fromList [("Access", TDNS (Node (Atom "gate") (Leaf LTop) (Leaf (Atom "close"))))])]}),
      testCase "If: Size relation - Less true" $
        evalTest (lang_1 ++ ifRelationSizeTrueLess) "id4" "id5"
          @?= Right (SuperPolicy {policies = [Policy (M.fromList [("Access", TDNS (Node (Atom "gate") (Leaf LTop) (Leaf (Atom "open"))))])]}),
      testCase "If: Size relation - Greater true" $
        evalTest (lang_1 ++ ifRelationSizeTrueGreater) "id4" "id5"
          @?= Right (SuperPolicy {policies = [Policy (M.fromList [("Access", TDNS (Node (Atom "gate") (Leaf LTop) (Leaf (Atom "open"))))])]}),
      testCase "If: Size relation - Equal true" $
        evalTest (lang_1 ++ ifRelationSizeTrueEqual) "id4" "id5"
          @?= Right (SuperPolicy {policies = [Policy (M.fromList [("Access", TDNS (Node (Atom "gate") (Leaf LTop) (Leaf (Atom "open"))))])]}),
      testCase "If: Size relation - Less false" $
        evalTest (lang_1 ++ ifRelationSizeFalseLess) "id4" "id5"
          @?= Right (SuperPolicy {policies = [Policy (M.fromList [("Access", TDNS (Node (Atom "gate") (Leaf LTop) (Leaf (Atom "close"))))])]}),
      testCase "If: Size relation - Greater false" $
        evalTest (lang_1 ++ ifRelationSizeFalseGreater) "id4" "id5"
          @?= Right (SuperPolicy {policies = [Policy (M.fromList [("Access", TDNS (Node (Atom "gate") (Leaf LTop) (Leaf (Atom "close"))))])]}),
      testCase "If: Size relation - Equal false" $
        evalTest (lang_1 ++ ifRelationSizeFalseEqual) "id4" "id5"
          @?= Right (SuperPolicy {policies = [Policy (M.fromList [("Access", TDNS (Node (Atom "gate") (Leaf LTop) (Leaf (Atom "close"))))])]}),
      testCase "If: Not relation - Eval true" $
        evalTest (lang_1 ++ ifRelationNotEvalTrue) "id4" "id5"
          @?= Right (SuperPolicy {policies = [Policy (M.fromList [("Access", TDNS (Node (Atom "gate") (Leaf LTop) (Leaf (Atom "open"))))])]}),
      testCase "If: Not relation - Eval false" $
        evalTest (lang_1 ++ ifRelationNotEvalFalse) "id4" "id5"
          @?= Right (SuperPolicy {policies = [Policy (M.fromList [("Access", TDNS (Node (Atom "gate") (Leaf LTop) (Leaf (Atom "close"))))])]}),
      testCase "If: Not relation - In true" $
        evalTest (lang_1 ++ ifRelationNotInTrue) "id4" "id5"
          @?= Right (SuperPolicy {policies = [Policy (M.fromList [("Access", TDNS (Node (Atom "gate") (Leaf LTop) (Leaf (Atom "open"))))])]}),
      testCase "If: Not relation - In false" $
        evalTest (lang_1 ++ ifRelationNotInFalse) "id4" "id5"
          @?= Right (SuperPolicy {policies = [Policy (M.fromList [("Access", TDNS (Node (Atom "gate") (Leaf LTop) (Leaf (Atom "close"))))])]}),
      testCase "If: Not relation - Size true" $
        evalTest (lang_1 ++ ifRelationNotSizeTrue) "id4" "id5"
          @?= Right (SuperPolicy {policies = [Policy (M.fromList [("Access", TDNS (Node (Atom "gate") (Leaf LTop) (Leaf (Atom "open"))))])]}),
      testCase "If: Not relation - Size false" $
        evalTest (lang_1 ++ ifRelationNotSizeFalse) "id4" "id5"
          @?= Right (SuperPolicy {policies = [Policy (M.fromList [("Access", TDNS (Node (Atom "gate") (Leaf LTop) (Leaf (Atom "close"))))])]}),
      testCase "Predicate" $
        evalTest (lang_1 ++ predicate) "id5" "id2"
          @?= Right (SuperPolicy {policies = [Policy (M.fromList [("Access", TDNS (Node (Atom "gate") (Leaf LTop) (Leaf (Atom "open"))))])]})
          -- Predicate sender 
          -- Predicate reciever
          -- for
          -- when
    ]

im = "import language.lan."

lan = "lang Tag_ {aspect1, aspect1[leaf1], aspect1.leaf2, aspect2@_-$}; lang Music {Pop, rock, rock.alternative, rock.heavy}."

expres = "trust(id1, id2) with {Tag: aspect1}; trust(id2, id3) with {Tag: aspect1[leaf1]}."

-- expres = "group One = [id1, id2, id3]; trust(id2, id3) with {Tag: aspect1[leaf1]}; trust(id1, id2) with {Tag: aspect1}."
exp_if = "group One = [id1, id2, id3]; if (id4 in One) then {trust(id1, id2) with {Tag: aspect1}} else {trust(id1, id2) with {Tag: aspect2}}."

exp_if_eval = "trust(id1, id2) with {Tag: aspect1}; if (eval(id1,id2: {Tag: aspect1})) then {trust(id1, id3) with {Tag: aspect1}.} else {trust(id1, id3) with {Tag: aspect2}.}."

exp_group = "group One = [id2, id3]; trust(One, id1) with {Tag: aspect1}."

exp_pred = "group One = [id2, id3]; trust(id1, One) with {Tag: aspect1}; group Two = pred X in {id1, X: {Tag: aspect1}}; trust(Two, id1) with {Tag: aspect2}."

exp_pred2 = "group One = [id2, id3]; trust(One, id1) with {Tag: aspect1}; group Two = pred X in {X, id1: {Tag: aspect1}}; trust(id1, Two) with {Tag: aspect2}."

exp_tmp = "policy Pol = {Tag: aspect1.leaf2}; trust(id1, id2) with Pol."

exp_for = "group One = [id2, id3];  trust(id1, One) with {Tag: aspect1}; for (X) where {id1, X: {Tag: aspect1}} do  {trust(X, id1) with {Tag: aspect2}.}."

exp_when_false = "when (eval(id1,id2: {Tag: aspect1})) do {trust(id2, id3) with {Tag: aspect1}} otherwise {trust(id2, id3) with {Tag: aspect2}}."

exp_when_true = "when (eval(id1,id2: {Tag: aspect1})) do {trust(id2, id3) with {Tag: aspect1}} otherwise {trust(id2, id3) with {Tag: aspect2}; trust(id1,id2) with {Tag: aspect1}}."

exp_trust = "trust(id2, id3) with {Tag_: aspect2@_-$}."

exp_trust_with_more = "trust(id2, id3) with {Tag_: aspect2@_-$}. trust(id4,id6) with {Tag_: aspect1}"

exp_empty = ""

simple = "lang Tag {aspect1}. trust(id1, id2) with {Tag: aspect1}."

test2 = "group Name = [group]."
