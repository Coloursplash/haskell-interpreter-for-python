module TokeniserTests (tokeniserTests) where

-- Assuming your Tokeniser module exists

import GHC.TypeError (Assert)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, testCase, (@?=))
import TestUtils (numberedTests)
import Tokeniser (tokenise)
import Types

tokeniserTests :: [TestTree]
tokeniserTests =
  [ testGroup "Tokenising simple expressions" (numberedTests tokeniseTests),
    testGroup "Tokenising all operators and delimiters" (numberedTests allOpsAndDelimsTests),
    testGroup "Tokenising all keywords" (numberedTests allKeywordsTests),
    testGroup "Tokenisation errors" (numberedTests errorTests),
    testGroup "Tokenising multiple lines" (numberedTests multiLineTests)
  ]

tokeniseTests :: [Assertion]
tokeniseTests =
  [ -- Basic arithmetic expressions
    tokenise "1 + 2" @?= Right [Val (Int 1), Operator Plus, Val (Int 2)],
    tokenise "x * y" @?= Right [Ident "x", Operator Times, Ident "y"],
    -- Function calls
    tokenise "print(42)" @?= Right [Ident "print", Delimiter LParen, Val (Int 42), Delimiter RParen],
    -- Operators with no spaces
    tokenise "5+-3" @?= Right [Val (Int 5), Operator Plus, Val (Int (-3))],
    tokenise "4**2" @?= Right [Val (Int 4), Operator Pow, Val (Int 2)],
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

allOpsAndDelimsTests :: [Assertion]
allOpsAndDelimsTests =
  [ tokenise "+ - * ** / // % @ << >> && | ^ ~ := < > <= >= == !="
      @?= Right
        [ Operator Plus,
          Operator Minus,
          Operator Times,
          Operator Pow,
          Operator DivOp,
          Operator IntDiv,
          Operator Mod,
          Operator At,
          Operator ShiftL,
          Operator ShiftR,
          Operator AndOp,
          Operator Pipe,
          Operator Hat,
          Operator Tilde,
          Operator AssignOp,
          Operator LessThan,
          Operator GreaterThan,
          Operator LTEq,
          Operator GTEq,
          Operator Eq,
          Operator NotEq
        ],
    tokenise "( ) [ ] { } , : ! . ; = ->"
      @?= Right
        [ Delimiter LParen,
          Delimiter RParen,
          Delimiter LSquare,
          Delimiter RSquare,
          Delimiter LBrace,
          Delimiter RBrace,
          Delimiter Comma,
          Delimiter Colon,
          Delimiter Exclamation,
          Delimiter Period,
          Delimiter Semi,
          Delimiter EqDelim,
          Delimiter ArrowRight
        ],
    tokenise "+= -= *= /= //= %= @= &= |= ^= >>= <<= **="
      @?= Right
        [ Delimiter PlusEq,
          Delimiter MinusEq,
          Delimiter TimesEq,
          Delimiter DivEq,
          Delimiter IntDivEq,
          Delimiter ModEq,
          Delimiter AtEq,
          Delimiter AndEq,
          Delimiter PipeEq,
          Delimiter HatEq,
          Delimiter ShiftREq,
          Delimiter ShiftLEq,
          Delimiter PowEq
        ]
  ]

allKeywordsTests :: [Assertion]
allKeywordsTests =
  [ tokenise "and as assert async await break class continue def del elif else except finally for from global if import in is lambda nonlocal not or pass raise return try while with yield"
      @?= Right
        [ Keyword And,
          Keyword As,
          Keyword Assert,
          Keyword Async,
          Keyword Await,
          Keyword Break,
          Keyword Class,
          Keyword Continue,
          Keyword Def,
          Keyword Del,
          Keyword Elif,
          Keyword Else,
          Keyword Except,
          Keyword Finally,
          Keyword For,
          Keyword From,
          Keyword Global,
          Keyword If,
          Keyword Import,
          Keyword In,
          Keyword Is,
          Keyword Lambda,
          Keyword Nonlocal,
          Keyword Not,
          Keyword Or,
          Keyword Pass,
          Keyword Raise,
          Keyword Return,
          Keyword Try,
          Keyword WhileTok,
          Keyword With,
          Keyword Yield
        ]
  ]

errorTests :: [Assertion]
errorTests =
  [ tokenise "#" @?= Left (TokenisationError (UnrecognizedOperator "#"))
  ]

multiLineTests :: [Assertion]
multiLineTests =
  [ tokenise "x=1\ny=2\nz=3*4\nx=3**y-z"
      @?= Right
        [ Ident "x",
          Delimiter EqDelim,
          Val (Int 1),
          Ident "y",
          Delimiter EqDelim,
          Val (Int 2),
          Ident "z",
          Delimiter EqDelim,
          Val (Int 3),
          Operator Times,
          Val (Int 4),
          Ident "x",
          Delimiter EqDelim,
          Val (Int 3),
          Operator Pow,
          Ident "y",
          Operator Minus,
          Ident "z"
        ]
  ]