module TokenizerTests (tokenizerTests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=), Assertion)
import Tokenizer (tokenize)  -- Assuming your Tokenizer module exists
import TestUtils (numberedTests)

tokenizerTests :: [TestTree]
tokenizerTests =
  [ testGroup "Tokenizing simple expressions" (numberedTests tokenizeTests) ]

tokenizeTests :: [Assertion]
tokenizeTests =
  [
--      tokenize "1 + 2" @?= [Number 1, Plus, Number 2]
--   , tokenize "x * y" @?= [Identifier "x", Mul, Identifier "y"]
--   , tokenize "print(42)" @?= [Identifier "print", LParen, Number 42, RParen]
  ]