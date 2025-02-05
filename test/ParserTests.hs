module ParserTests (parserTests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=), Assertion)
import Parser (parse)  -- Assuming your Parser module exists
import AST (Expr(..))  -- Assuming your AST module exists
import TestUtils (numberedTests)

parserTests :: [TestTree]
parserTests =
  [ testGroup "Parsing expressions" (numberedTests parseTests) ]

parseTests :: [Assertion]
parseTests =
  [
--     parse [Number 1, Plus, Number 2] @?= Add (Literal 1) (Literal 2)
--   , parse [Identifier "x", Mul, Identifier "y"]
--       @?= Mul (Var "x") (Var "y")
  ]