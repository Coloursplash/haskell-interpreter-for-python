module EvaluatorTests (evaluatorTests) where

import Evaluator (evaluate) -- Assuming your Evaluator module exists
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, testCase, (@?=))
import TestUtils (numberedTests)

evaluatorTests :: [TestTree]
evaluatorTests =
  [testGroup "Evaluating expressions" (numberedTests evaluatorTests')]

evaluatorTests' :: [Assertion]
evaluatorTests' =
  []
