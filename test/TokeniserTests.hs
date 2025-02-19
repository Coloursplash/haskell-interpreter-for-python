module TokeniserTests (tokeniserTests) where

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
    testGroup "Tokenising multiple lines" (numberedTests multiLineTests),
    testGroup "Tokenising short programs" (numberedTests shortProgramTests),
    testGroup "Tokenising different value types" (numberedTests valueTypeTests),
    testGroup "Tokenising indentation" (numberedTests indentationTests)
  ]

tokeniseTests :: [Assertion]
tokeniseTests =
  [ tokenise "1 + 2" @?= Right [Val (Int 1), Operator Plus, Val (Int 2)],
    tokenise "x * y" @?= Right [Ident "x", Operator Times, Ident "y"],
    tokenise "print(42)" @?= Right [Ident "print", Delimiter LParen, Val (Int 42), Delimiter RParen],
    tokenise "5+-3" @?= Right [Val (Int 5), Operator Plus, Val (Int (-3))],
    tokenise "4**2" @?= Right [Val (Int 4), Operator Pow, Val (Int 2)],
    tokenise "x+=1" @?= Right [Ident "x", Delimiter PlusEq, Val (Int 1)],
    tokenise "y<<=2" @?= Right [Ident "y", Delimiter ShiftLEq, Val (Int 2)],
    tokenise "a>>=b" @?= Right [Ident "a", Delimiter ShiftREq, Ident "b"],
    tokenise "c||d" @?= Right [Ident "c", Operator Pipe, Operator Pipe, Ident "d"],
    tokenise "((x))" @?= Right [Delimiter LParen, Delimiter LParen, Ident "x", Delimiter RParen, Delimiter RParen],
    tokenise "{[()]}->x" @?= Right [Delimiter LBrace, Delimiter LSquare, Delimiter LParen, Delimiter RParen, Delimiter RSquare, Delimiter RBrace, Delimiter ArrowRight, Ident "x"],
    tokenise "a+=b*c-d/e" @?= Right [Ident "a", Delimiter PlusEq, Ident "b", Operator Times, Ident "c", Operator Minus, Ident "d", Operator DivOp, Ident "e"],
    tokenise "a! = b" @?= Right [Ident "a", Delimiter Exclamation, Delimiter EqDelim, Ident "b"],
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
  [ tokenise "#" @?= Left (TokenisationError (UnrecognizedOperator "#")),
    tokenise "?" @?= Left (TokenisationError (UnrecognizedOperator "?"))
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

shortProgramTests :: [Assertion]
shortProgramTests =
  [ tokenise "def fib(n):\n    if n <= 1:\n        return n\n    else:\n        return fib(n - 1) + fib(n - 2)"
      @?= Right
        [ Keyword Def,
          Ident "fib",
          Delimiter LParen,
          Ident "n",
          Delimiter RParen,
          Delimiter Colon,
          BlockStart,
          Keyword If,
          Ident "n",
          Operator LTEq,
          Val (Int 1),
          Delimiter Colon,
          BlockStart,
          Keyword Return,
          Ident "n",
          BlockEnd,
          Keyword Else,
          Delimiter Colon,
          BlockStart,
          Keyword Return,
          Ident "fib",
          Delimiter LParen,
          Ident "n",
          Operator Minus,
          Val (Int 1),
          Delimiter RParen,
          Operator Plus,
          Ident "fib",
          Delimiter LParen,
          Ident "n",
          Operator Minus,
          Val (Int 2),
          Delimiter RParen,
          BlockEnd,
          BlockEnd
        ],
    tokenise "for i in range(10):\n    print(i)"
      @?= Right
        [ Keyword For,
          Ident "i",
          Keyword In,
          Ident "range",
          Delimiter LParen,
          Val (Int 10),
          Delimiter RParen,
          Delimiter Colon,
          BlockStart,
          Ident "print",
          Delimiter LParen,
          Ident "i",
          Delimiter RParen,
          BlockEnd
        ]
  ]

valueTypeTests :: [Assertion]
valueTypeTests =
  [ tokenise "42" @?= Right [Val (Int 42)],
    tokenise "3.14" @?= Right [Val (Float 3.14)],
    tokenise "\"Hello, World!\"" @?= Right [Val (Str "Hello, World!")],
    tokenise "True" @?= Right [Val TrueVal],
    tokenise "False" @?= Right [Val FalseVal],
    tokenise "None" @?= Right [Val NoneVal]
  ]

indentationTests :: [Assertion]
indentationTests =
  [ tokenise "if True:\n    x = 1\n    y = 2\nelse:\n    z = 3"
      @?= Right
        [ Keyword If,
          Val TrueVal,
          Delimiter Colon,
          BlockStart,
          Ident "x",
          Delimiter EqDelim,
          Val (Int 1),
          Ident "y",
          Delimiter EqDelim,
          Val (Int 2),
          BlockEnd,
          Keyword Else,
          Delimiter Colon,
          BlockStart,
          Ident "z",
          Delimiter EqDelim,
          Val (Int 3),
          BlockEnd
        ],
    tokenise "def foo():\n    if x:\n        y()\n    else:\n        z()"
      @?= Right
        [ Keyword Def,
          Ident "foo",
          Delimiter LParen,
          Delimiter RParen,
          Delimiter Colon,
          BlockStart,
          Keyword If,
          Ident "x",
          Delimiter Colon,
          BlockStart,
          Ident "y",
          Delimiter LParen,
          Delimiter RParen,
          BlockEnd,
          Keyword Else,
          Delimiter Colon,
          BlockStart,
          Ident "z",
          Delimiter LParen,
          Delimiter RParen,
          BlockEnd,
          BlockEnd
        ]
  ]
