module TokeniserTests (tokeniserTests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, testCase, (@?=))
-- Assuming your Tokeniser module exists
import TestUtils (numberedTests)
import Tokeniser (tokenise)
import Types

tokeniserTests :: [TestTree]
tokeniserTests =
  [testGroup "Tokenizing simple expressions" (numberedTests tokeniseTests)]

tokeniseTests :: [Assertion]
tokeniseTests =
  [ tokenise "1 + 2" @?= Right [Val (Int 1), Operator Plus, Val (Int 2)],
    tokenise "x * y" @?= Right [Ident "x", Operator Times, Ident "y"],
    tokenise "print(42)" @?= Right [Ident "print", Delimiter LParen, Val (Int 42), Delimiter RParen]
  ]
