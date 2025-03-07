module EvaluatorTests (evaluatorTests) where

import Control.Monad.Trans.Except (runExceptT)
import Evaluator (evaluate)
import System.IO.Unsafe (unsafePerformIO)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, testCase, (@?=))
import TestUtils (numberedTests)
import Types

evaluatorTests :: [TestTree]
evaluatorTests =
  [ testGroup "Evaluating assignments" (numberedTests assignmentsTests),
    testGroup "Evaluating basic expressions" (numberedTests basicExprTests)
  ]

evalHelp :: Block -> VarList
evalHelp b = case unsafePerformIO $ runExceptT $ evaluate b of
  Left _ -> [("%error%", Int 0)]
  Right varList -> varList

assignmentsTests :: [Assertion]
assignmentsTests =
  [ evalHelp
      [ Asgn "x" (ValExp $ List [ValExp $ Int 1, ValExp $ Int 2, ValExp $ Int 3])
      ]
      @?= [("x", List [ValExp $ Int 1, ValExp $ Int 2, ValExp $ Int 3])],
    evalHelp
      [Asgn "x" (ValExp $ Int 0), Cond (Eq (Identifier "x") (ValExp $ Int 0)) [Asgn "x" (ValExp $ Int 5)] [Asgn "x" (ValExp $ Int 10)]]
      @?= [("x", Int 5)],
    evalHelp
      [Asgn "x" (ValExp $ Int 0), While (LessThan (Identifier "x") (ValExp $ Int 10)) [Asgn "x" (Add (Identifier "x") (ValExp $ Int 1))]]
      @?= [("x", Int 10)],
    evalHelp
      [ Asgn "x" (ValExp $ Str "Hello"),
        Asgn "y" (Add (Identifier "x") (ValExp $ Str " world!")),
        Asgn "z" (MethodCall (Identifier "y") "get" [ValExp $ Int 11])
      ]
      @?= [("z", Str "!"), ("y", Str "Hello world!"), ("x", Str "Hello")]
  ]

basicExprTests :: [Assertion]
basicExprTests =
  [ evalHelp
      [ Asgn "x" (ValExp $ Bool True),
        Asgn "y" (NotExp (Identifier "x"))
      ]
      @?= [("y", Bool False), ("x", Bool True)],
    evalHelp
      [ Asgn "x" (ValExp $ List [ValExp $ Int 10, ValExp $ Int 20, ValExp $ Int 30]),
        Asgn "y" (MethodCall (Identifier "x") "get" [ValExp $ Int 1])
      ]
      @?= [("y", Int 20), ("x", List [ValExp $ Int 10, ValExp $ Int 20, ValExp $ Int 30])],
    evalHelp
      [ Asgn "x" (ValExp $ Int 3),
        Asgn "y" (Mul (Identifier "x") (ValExp $ Int 5))
      ]
      @?= [("y", Int 15), ("x", Int 3)],
    evalHelp
      [ Asgn "x" (ValExp $ Int 1),
        Asgn "y" (ValExp $ Int 2),
        Cond
          (GreaterThan (Identifier "x") (Identifier "y"))
          [Asgn "z" (ValExp $ Str "greater")]
          [Asgn "z" (ValExp $ Str "less")]
      ]
      @?= [("z", Str "less"), ("y", Int 2), ("x", Int 1)],
    evalHelp
      [ Asgn "x" (ValExp $ Int 0),
        While
          (LessThan (Identifier "x") (ValExp $ Int 3))
          [ Asgn "x" (Add (Identifier "x") (ValExp $ Int 1)),
            Asgn "y" (Mul (Identifier "x") (ValExp $ Int 2))
          ]
      ]
      @?= [("y", Int 6), ("x", Int 3)],
    evalHelp
      [ Asgn "x" (FunctionCall "range" [ValExp $ Int 5]),
        Asgn "y" (MethodCall (Identifier "x") "get" [ValExp $ Int 2])
      ]
      @?= [("y", Int 2), ("x", List [ValExp $ Int 0, ValExp $ Int 1, ValExp $ Int 2, ValExp $ Int 3, ValExp $ Int 4])],
    evalHelp
      [ Asgn "x" (ValExp $ Int 1),
        Cond
          (Eq (Identifier "x") (ValExp $ Int 1))
          [ Cond
              (Eq (Identifier "x") (ValExp $ Int 2))
              [Asgn "y" (ValExp $ Int 100)]
              [Asgn "y" (ValExp $ Int 200)]
          ]
          [Asgn "y" (ValExp $ Int 300)]
      ]
      @?= [("y", Int 200), ("x", Int 1)]
  ]
