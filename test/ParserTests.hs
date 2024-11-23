-- Blackbox testing of parser
{-# LANGUAGE OverloadedStrings        #-}
{-# LANGUAGE DisambiguateRecordFields #-}

import AST
-- import RunTime
import Parser

import Parser.ParserImpl
import qualified Text.Megaparsec  as P
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import Test.Tasty
import Test.Tasty.HUnit
import qualified Data.Text as T



import Data.Either (isLeft)
import qualified Data.Map.Strict as M
convert (name, input, mout) f = testCase name $ case mout of
    Nothing  -> isLeft (f input) @?= True
    Just out -> f input @?= Right out

convert' (name, input, output) f = testCase name $ f input @?= output


-- main :: IO ()
-- main = defaultMain $ localOption (mkTimeout 1000000) tests

-- tests = testGroup "Parser tests" [
--     testCase "parse empty program" $ runNetworkParser "import file.lan." (T.pack "import file.lan.") @?=
--         Right (Network [Imp "file.lan"] (Language [])),
--     testCase "parse empty program" $ runNetworkParser "empty" (T.pack "\n") @?=
--         Right (Network {imp = [], lang = Language {langDef = []}}),
--     testCase "parse import ttest" $ runImportParser "import file.lan" (T.pack "import file.lan") @?=
--         Right (Imp "file.lan")
--     ]  
tests :: [TestTree]
tests = 
    [
        testGroup "Network parser" $ map (`convert` runImportParser "test") importCases
    ]

importCases = 
    [
        ("Simple import", "import file.lan.", Just $ Imp {file = "file.lan"}),
        ("Empty import", "", Just $ Imp {file = "file.lan"})
    ]

test = testGroup "Unit tests" $ concat
    [ tests ]

main = defaultMain test

-- "import file.lan; import file1.lan;import file4.lan;   import file3.lan . "
-- "lang Tag {test, test[gt], test.test, one[two].three}"
-- "lang Tag {fst}; lang Tag2 {snd}."