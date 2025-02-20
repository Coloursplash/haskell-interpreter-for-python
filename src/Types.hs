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
  | PowOp
  | DivOp
  | IntDivOp
  | ModOp
  | AtOp
  | ShiftLOp
  | ShiftROp
  | AndOp
  | PipeOp
  | HatOp
  | TildeOp
  | AssignOp
  | LessThanOp
  | GreaterThanOp
  | LTEqOp
  | GTEqOp
  | EqOp
  | NotEqOp
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
  = Int Integer
  | Float Float
  | Str String
  | Bool Bool
  | FalseVal
  | NoneVal
  | TrueVal
  deriving (Eq, Show)

type Program = Block

type Block = [Stmt]

data Stmt
  = Asgn String Expr
  | While Expr Block
  | Cond Expr Block Block
  | FunctionCall String Expr
  | ExprStmt Expr
  deriving (Eq, Show)

data Expr
  = ValExp Val
  | Add Expr Expr
  | Sub Expr Expr
  | Mul Expr Expr
  | Pow Expr Expr
  | Div Expr Expr
  | IntDiv Expr Expr 
  | Mod Expr Expr
  | At Expr Expr
  | ShiftL Expr Expr 
  | ShiftR Expr Expr 
  | AndExp Expr Expr 
  | Pipe Expr Expr
  | Hat Expr Expr 
  | Tilde Expr Expr 
  | Assign Expr Expr 
  | LessThan Expr Expr 
  | GreaterThan Expr Expr 
  | LTEq Expr Expr 
  | GTEq Expr Expr 
  | Eq Expr Expr 
  | NotEq Expr Expr 
  | Identifier String
  deriving (Eq, Show)
