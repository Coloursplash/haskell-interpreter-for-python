module TokeniserTests (tokeniserTests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=), Assertion)
import Tokeniser (tokenise)  -- Assuming your Tokeniser module exists
import TestUtils (numberedTests)

tokeniserTests :: [TestTree]
tokeniserTests =
  [ testGroup "Tokenizing simple expressions" (numberedTests tokeniseTests) ]

tokeniseTests :: [Assertion]
tokeniseTests =
  [
--      tokenise "1 + 2" @?= [Number 1, Plus, Number 2]
--   , tokenise "x * y" @?= [Identifier "x", Mul, Identifier "y"]
--   , tokenise "print(42)" @?= [Identifier "print", LParen, Number 42, RParen]
  ]