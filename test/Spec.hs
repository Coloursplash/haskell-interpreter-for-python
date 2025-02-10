import EvaluatorTests (evaluatorTests)
import ParserTests (parserTests)
import Test.Tasty (TestTree, defaultMain, testGroup)
import TokeniserTests (tokeniserTests)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup
    "Python Interpreter Tests"
    [ testGroup "Tokeniser" tokeniserTests,
      testGroup "Parser" parserTests,
      testGroup "Evaluator" evaluatorTests
    ]