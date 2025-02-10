module TestUtils (numberedTests) where

import Test.Tasty (TestTree)
import Test.Tasty.HUnit (Assertion, testCase)

numberedTests :: [Assertion] -> [TestTree]
numberedTests tests = zipWith makeTest [1 :: Int ..] tests
  where
    makeTest i assertion = testCase ("Test " ++ show i) assertion