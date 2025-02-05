import Test.Tasty (defaultMain, TestTree, testGroup)
import TokenizerTests (tokenizerTests)
import ParserTests (parserTests)
import EvaluatorTests (evaluatorTests)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Python Interpreter Tests"
  [ testGroup "Tokenizer" tokenizerTests
  , testGroup "Parser" parserTests
  , testGroup "Evaluator" evaluatorTests
  ]