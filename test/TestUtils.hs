module TestUtils (numberedTests) where

import Test.Tasty (TestTree)
import Test.Tasty.HUnit (Assertion, testCase)

numberedTests :: [Assertion] -> [TestTree]
numberedTests = zipWith makeTest [1 :: Int ..]
  where
    makeTest i = testCase ("Test " ++ show i)