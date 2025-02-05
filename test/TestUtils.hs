module TestUtils (numberedTests) where

import Test.Tasty.HUnit (testCase, Assertion)
import Test.Tasty (TestTree)

numberedTests :: [Assertion] -> [TestTree]
numberedTests tests = zipWith makeTest [1 :: Int ..] tests
  where
    makeTest i assertion = testCase ("Test " ++ show i) assertion