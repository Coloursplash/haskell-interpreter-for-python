module Types where

type Through a b = a -> Either Error b

data Error
  = FileError FileError
  | TokenisationError TokenisationError
  | ParsingError ParsingError
  | EvaluationError EvaluationError
  deriving (Eq, Show)

-- File errors for Main.hs
data FileError
  = NoFilePathProvided
  | FileNotFound String
  deriving (Eq, Show)

-- Tokenization errors
data TokenisationError
  = BadChar Char
  | UnrecognizedOperator String
  | UnexpectedEOF
  deriving (Eq, Show)

-- Parsing errors
data ParsingError
  = StmtNotFound (Maybe Token)
  | ExprNotFound (Maybe Token)
  | UnparsedInput [Token]
  | SyntaxError String
  | IndentationError String
  | Unexpected (Maybe Token) Token
  deriving (Eq, Show)

-- Evaluation errors
data EvaluationError
  = TypeError String
  | NameError String
  | AttributeError String
  | IndexError String
  | KeyError String
  | ZeroDivisionError String
  | RuntimeError String
  | IOError String
  | InvalidOperation String
  deriving (Eq, Show)

data Token
  = Keyword Keyword
  | Operator Operator
  | Delimiter Delimiter
  -- Since python uses indentation for blocks
  | BlockStart
  | BlockEnd
  | Val Val
  | Ident String -- Can be variable or function name
  deriving (Eq, Show)

data Keyword
  = And
  | As
  | Assert
  | Async
  | Await
  | Break
  | Class
  | Continue
  | Def
  | Del
  | Elif
  | Else
  | Except
  | Finally
  | For
  | From
  | Global
  | If
  | Import
  | In
  | Is
  | Lambda
  | Nonlocal
  | Not
  | Or
  | Pass
  | Raise
  | Return
  | Try
  | WhileTok
  | With
  | Yield
  deriving (Eq, Show)

data Operator
  = Plus
  | Minus
  | Times
  | Pow
  | DivOp
  | IntDiv
  | Mod
  | At
  | ShiftL
  | ShiftR
  | AndOp
  | Pipe
  | Hat
  | Tilde
  | AssignOp
  | LessThan
  | GreaterThan
  | LTEq
  | GTEq
  | Eq
  | NotEq
  deriving (Eq, Show)

data Delimiter
  = LParen
  | RParen
  | LSquare
  | RSquare
  | LBrace
  | RBrace
  | Comma
  | Colon
  | Exclamation
  | Period
  | Semi
  | AtDelim
  | EqDelim
  | ArrowRight
  | PlusEq
  | MinusEq
  | TimesEq
  | DivEq
  | IntDivEq
  | ModEq
  | AtEq
  | AndEq
  | PipeEq
  | HatEq
  | ShiftREq
  | ShiftLEq
  | PowEq
  deriving (Eq, Show)

data Val
  = Int Int
  | Str String
  | Bool Bool
  | FalseVal
  | None
  | TrueVal
  deriving (Eq, Show)

type Program = Block

type Block = [Stmt]

data Stmt
  = Asgn String Expr
  | While Expr Block
  | FunctionCall String
  deriving (Eq, Show)

data Expr
  = Num Int
  | Add Expr Expr
  | Sub Expr Expr
  | Mul Expr Expr
  | Div Expr Expr
  | Identifier String
  deriving (Eq, Show)
