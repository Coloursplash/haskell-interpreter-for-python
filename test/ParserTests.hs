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
  [ testGroup "Parsing expressions" (numberedTests parseTests),
    testGroup "Additional parsing tests" (numberedTests additionalParseTests),
    testGroup "Parsing Error Test" (numberedTests parserErrorTests)
  ]

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
              [Print (ValExp (Str "positive"))]
              [Print (ValExp (Str "negative"))]]
  , parse [Ident "print", Delimiter LParen, Val (Str "Hello, World!"), Delimiter RParen] 
    @?= Right [Print (ValExp (Str "Hello, World!"))]
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
  , parse [Operator Plus] @?= Left (ParsingError (ExprNotFound (Just (Operator Plus))))
  ]

additionalParseTests :: [Assertion]
additionalParseTests =
  [ -- Nested function calls
    parse [Ident "print", Delimiter LParen, Ident "str", Delimiter LParen, Ident "len", Delimiter LParen, Ident "exampleString", Delimiter RParen, Delimiter RParen, Delimiter RParen]
    @?= Right [Print (FunctionCall "str" [FunctionCall "len" [Identifier "exampleString"]])]

  -- Multiple nested conditions
  , parse [Keyword If, Ident "x", Operator GreaterThanOp, Val (Int 0), Delimiter Colon, BlockStart,
           Keyword If, Ident "y", Operator LessThanOp, Val (Int 10), Delimiter Colon, BlockStart,
           Ident "print", Delimiter LParen, Val (Str "a"), Delimiter RParen, BlockEnd,
           Keyword Else, Delimiter Colon, BlockStart,
           Ident "print", Delimiter LParen, Val (Str "b"), Delimiter RParen, BlockEnd,
           BlockEnd,
           Keyword Else, Delimiter Colon, BlockStart,
           Ident "print", Delimiter LParen, Val (Str "c"), Delimiter RParen, BlockEnd]
    @?= Right [Cond (GreaterThan (Identifier "x") (ValExp (Int 0)))
               [Cond (LessThan (Identifier "y") (ValExp (Int 10)))
                [Print (ValExp (Str "a"))]
                [Print (ValExp (Str "b"))]]
               [Print (ValExp (Str "c"))]]

  -- Complex arithmetic expression with parentheses
  , parse [Ident "result", Delimiter EqDelim, 
           Delimiter LParen, Val (Int 1), Operator Plus, Val (Int 2), Delimiter RParen,
           Operator Times,
           Delimiter LParen, Val (Int 3), Operator Minus, 
           Delimiter LParen, Val (Int 4), Operator DivOp, Val (Int 2), Delimiter RParen,
           Delimiter RParen]
    @?= Right [Asgn "result" (Mul 
                (Add (ValExp (Int 1)) (ValExp (Int 2)))
                (Sub (ValExp (Int 3)) (Div (ValExp (Int 4)) (ValExp (Int 2)))))]

  -- While loop with complex condition
  , parse [Keyword WhileTok, 
           Delimiter LParen, Ident "x", Operator GreaterThanOp, Val (Int 0), Delimiter RParen,
           Operator AndOp,
           Delimiter LParen, Ident "y", Operator LessThanOp, Val (Int 10), Delimiter RParen,
           Delimiter Colon, BlockStart,
           Ident "x", Delimiter MinusEq, Val (Int 1),
           Ident "y", Delimiter PlusEq, Val (Int 1),
           BlockEnd]
    @?= Right [While (AndExp 
                (GreaterThan (Identifier "x") (ValExp (Int 0)))
                (LessThan (Identifier "y") (ValExp (Int 10))))
               [Asgn "x" (Sub (Identifier "x") (ValExp (Int 1))),
                Asgn "y" (Add (Identifier "y") (ValExp (Int 1)))]]
    , parse [Keyword If, 
           Delimiter LParen, Ident "x", Operator GreaterThanOp, Val (Int 0), Delimiter RParen,
           Operator AndOp,
           Delimiter LParen, Ident "y", Operator LessThanOp, Val (Int 10), Delimiter RParen,
           Delimiter Colon, BlockStart,
           Ident "x", Delimiter MinusEq, Val (Int 1),
           Ident "y", Delimiter PlusEq, Val (Int 1),
           BlockEnd, Keyword Else, Delimiter Colon, BlockStart,
           Ident "x", Delimiter MinusEq, Val (Int 2),
           Ident "y", Delimiter PlusEq, Val (Int 2), BlockEnd
           ]
    @?= Right [Cond (AndExp 
                (GreaterThan (Identifier "x") (ValExp (Int 0)))
                (LessThan (Identifier "y") (ValExp (Int 10))))
               [Asgn "x" (Sub (Identifier "x") (ValExp (Int 1))),
                Asgn "y" (Add (Identifier "y") (ValExp (Int 1)))]
               [Asgn "x" (Sub (Identifier "x") (ValExp (Int 2))),
                Asgn "y" (Add (Identifier "y") (ValExp (Int 2)))
               ]]
  ]

parserErrorTests :: [Assertion]
parserErrorTests = [
  -- Incomplete assignment
    parse [Ident "x", Delimiter EqDelim]
    @?= Left (ParsingError (ExprNotFound (Just BlockEnd)))

  -- Missing closing parenthesis
  , parse [Ident "print", Delimiter LParen, Val(Str "Hello")]
    @?= Left (ParsingError (Unexpected (Just BlockEnd) (Delimiter RParen)))

  -- Unexpected token in expression
  , parse [Val (Int 5), Operator Plus, Keyword If]
    @?= Left (ParsingError (ExprNotFound (Just (Keyword If))))

  -- Mismatched block structure
  , parse [Keyword If, Ident "x", Operator GreaterThanOp, Val (Int 0), Delimiter Colon, BlockStart,
           Ident "print", Delimiter LParen, Val(Str "Positive"), Delimiter RParen]
    @?= Left (ParsingError (Unexpected Nothing (Keyword Else)))
      --Left (ParsingError (Unexpected Nothing BlockEnd))? 
      -- depends but this error would be much harder to do with the current 
      -- implementation

  -- Invalid operator in expression
  , parse [Ident "y", Delimiter EqDelim, Val (Int 1), Delimiter Comma, Val (Int 2)]
    @?= Left (ParsingError (ExprNotFound (Just (Delimiter Comma))))

  -- Unexpected end of input
  , parse [Keyword WhileTok, Ident "True", Delimiter Colon]
    @?= Left (ParsingError (Unexpected (Just BlockEnd) BlockStart))

  -- Invalid token sequence
  , parse [Operator Plus, Operator Minus, Operator Times]
    @?= Left (ParsingError (ExprNotFound (Just (Operator Plus))))

  -- Unexpected keyword
  , parse [Ident "x", Delimiter EqDelim, Keyword Else]
    @?= Left (ParsingError (ExprNotFound (Just (Keyword Else))))

  -- Invalid function call
  , parse [Ident "func", Delimiter LParen, Delimiter RParen, Delimiter LParen]
    @?= Left (ParsingError (ExprNotFound (Just (Delimiter RParen))))

  -- Incomplete if-else structure
  , parse [Keyword If, Ident "x", Operator GreaterThanOp, Val (Int 0), Delimiter Colon, BlockStart,
           Ident "print", Delimiter LParen, Val (Str "Positive"), Delimiter RParen, BlockEnd,
           Keyword Else]
    @?= Left (ParsingError (Unexpected (Just BlockEnd) (Delimiter Colon)))

  -- Invalid indentation (if your parser checks for this)
  , parse [BlockStart, BlockStart, Ident "x", Delimiter EqDelim, Val (Int 5), BlockEnd]
    @?= Left (ParsingError (ExprNotFound (Just BlockStart)))

  -- Invalid use of keywords
  , parse [Keyword If, Keyword Else, Delimiter Colon]
    @?= Left (ParsingError (ExprNotFound (Just (Keyword Else))))

  -- Incomplete binary operation
  , parse [Val (Int 5), Operator Plus]
    @?= Left (ParsingError (ExprNotFound (Just BlockEnd)))
  ]