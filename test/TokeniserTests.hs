module TokeniserTests (tokeniserTests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, testCase, (@?=))
-- Assuming your Tokeniser module exists
import TestUtils (numberedTests)
import Tokeniser (tokenise)
import Types

tokeniserTests :: [TestTree]
tokeniserTests =
  [testGroup "Tokenizing simple expressions" (numberedTests tokeniseTests)]

tokeniseTests :: [Assertion]
tokeniseTests =
  [ -- Basic arithmetic expressions
    tokenise "1 + 2" @?= Right [Val (Int 1), Operator Plus, Val (Int 2)],
    tokenise "x * y" @?= Right [Ident "x", Operator Times, Ident "y"],
    -- Function calls
    tokenise "print(42)" @?= Right [Ident "print", Delimiter LParen, Val (Int 42), Delimiter RParen],
    -- Operators with no spaces
    tokenise "5+-3" @?= Right [Val (Int 5), Operator Plus, Operator Minus, Val (Int (-3))],
    tokenise "4**2" @?= Right [Val (Int 4), Operator Times, Operator Times, Val (Int 2)],
    tokenise "x+=1" @?= Right [Ident "x", Delimiter PlusEq, Val (Int 1)],
    tokenise "y<<=2" @?= Right [Ident "y", Delimiter ShiftLEq, Val (Int 2)],
    -- Chained operators
    tokenise "a>>=b" @?= Right [Ident "a", Delimiter ShiftREq, Ident "b"],
    tokenise "c||d" @?= Right [Ident "c", Operator Pipe, Operator Pipe, Ident "d"],
    -- Nested brackets and mixed delimiters
    tokenise "((x))" @?= Right [Delimiter LParen, Delimiter LParen, Ident "x", Delimiter RParen, Delimiter RParen],
    tokenise "{[()]}->x" @?= Right [Delimiter LBrace, Delimiter LSquare, Delimiter LParen, Delimiter RParen, Delimiter RSquare, Delimiter RBrace, Delimiter ArrowRight, Ident "x"],
    -- Complex expressions with multiple operators
    tokenise "a+=b*c-d/e" @?= Right [Ident "a", Delimiter PlusEq, Ident "b", Operator Times, Ident "c", Operator Minus, Ident "d", Operator DivOp, Ident "e"],
    -- Edge case: single-character operator followed by a different one
    tokenise "a! = b" @?= Right [Ident "a", Delimiter Exclamation, Delimiter EqDelim, Ident "b"],
    -- Testing equality and comparison operators
    tokenise "x==y" @?= Right [Ident "x", Operator Eq, Ident "y"],
    tokenise "a!=b" @?= Right [Ident "a", Operator NotEq, Ident "b"],
    tokenise "5>=4" @?= Right [Val (Int 5), Operator GTEq, Val (Int 4)],
    tokenise "3<=2" @?= Right [Val (Int 3), Operator LTEq, Val (Int 2)]
  ]
