module ParserTests (parserTests) where

import Parser (parse) -- Assuming your Parser module exists
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, testCase, (@?=))
import TestUtils (numberedTests)
import Data.Either(fromRight)
import Tokeniser(tokenise)
import Types

parserTests :: [TestTree]
parserTests =
  [testGroup "Parsing expressions" (numberedTests parseTests)]

parseTests :: [Assertion]
parseTests =
  [ parse [Ident "x", Delimiter EqDelim, Val (Int 5)] 
    @?= Right [Asgn "x" (ValExp (Int 5))]
  , parse [Ident "y", Delimiter EqDelim, Val (Int 1), Operator Plus, Val (Int 2)] 
    @?= Right [Asgn "y" (Add (ValExp (Int 1)) (ValExp (Int 2)))]
  , parse [Keyword WhileTok, Ident "x", Operator LessThanOp, Val (Int 10), Delimiter Colon, BlockStart, 
          Ident "x", Delimiter PlusEq, Val (Int 1), BlockEnd] 
    @?= Right [While (LessThan (Identifier "x") (ValExp (Int 10))) 
              [Asgn "x" (Add (Identifier "x") (ValExp (Int 1)))]]
  , parse [Keyword If, Ident "x", Operator GTEqOp, Val (Int 0), Delimiter Colon, BlockStart, 
           Ident "print", Delimiter LParen, Val (Str "positive"), Delimiter RParen, BlockEnd, 
           Keyword Else, Delimiter Colon, BlockStart, 
           Ident "print", Delimiter LParen, Val (Str "negative"), Delimiter RParen, BlockEnd] 
    @?= Right [Cond (GTEq (Identifier "x") (ValExp (Int 0))) 
              [FunctionCall "print" (ValExp (Str "positive"))]
              [FunctionCall "print" (ValExp (Str "negative"))]]
  , parse [Ident "print", Delimiter LParen, Val (Str "Hello, World!"), Delimiter RParen] 
    @?= Right [FunctionCall "print" (ValExp (Str "Hello, World!"))]
  , parse [Ident "result", Delimiter EqDelim, 
           Val (Int 2), Operator PowOp, Delimiter LParen, 
           Val (Int 3), Operator Plus, Val (Int 4), Operator Times, Val (Int 5), 
           Delimiter RParen] 
    @?= Right [Asgn "result" (Pow (ValExp (Int 2)) 
               (Add (ValExp (Int 3)) (Mul (ValExp (Int 4)) (ValExp (Int 5)))))]

  , parse [Ident "x", Delimiter EqDelim, Val (Int 10), 
           Ident "y", Delimiter EqDelim, Val (Int 20), 
           Ident "z", Delimiter EqDelim, Ident "x", Operator Plus, Ident "y"] 
    @?= Right [ Asgn "x" (ValExp (Int 10))
              , Asgn "y" (ValExp (Int 20))
              , Asgn "z" (Add (Identifier "x") (Identifier "y"))
              ]
  , parse [] @?= Right []
  , parse [Operator Plus] @?= Left (ParsingError (StmtNotFound (Just (Operator Plus))))
  ]
