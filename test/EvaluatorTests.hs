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

--     evaluate (Add (Literal 1) (Literal 2)) @?= 3
--   , evaluate (Mul (Literal 3) (Literal 4)) @?= 12
