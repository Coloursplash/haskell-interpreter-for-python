module ParserTests (parserTests) where

import Parser (parse) -- Assuming your Parser module exists
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, testCase, (@?=))
import TestUtils (numberedTests)

parserTests :: [TestTree]
parserTests =
  [testGroup "Parsing expressions" (numberedTests parseTests)]

parseTests :: [Assertion]
parseTests =
  []
