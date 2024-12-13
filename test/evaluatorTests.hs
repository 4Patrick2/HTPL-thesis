-- Blackbox testing of evaluator
{-# LANGUAGE OverloadedStrings        #-}

import AST
import Main

import Test.Tasty
import Test.Tasty.HUnit
import qualified Data.Text as T
import qualified Data.Map.Strict as M

main :: IO ()
main = defaultMain $ localOption (mkTimeout 1000000) tests

-- tests :: TestTree
tests = testGroup "HTPL - Evaluator Testing:" []

delegationTests :: TestTree
delegationTests = testGroup "Delegation:" [
    testCase "Empty network" $ test "" "id1" "id2" @?=
        Right (Network {imp = [], lang = Language {langDef = M.fromList []}, exps = []}),
    ]   