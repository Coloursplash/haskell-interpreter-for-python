import Test.Tasty (defaultMain, TestTree, testGroup)
import TokeniserTests (tokeniserTests)
import ParserTests (parserTests)
import EvaluatorTests (evaluatorTests)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Python Interpreter Tests"
  [ testGroup "Tokeniser" tokeniserTests
  , testGroup "Parser" parserTests
  , testGroup "Evaluator" evaluatorTests
  ]