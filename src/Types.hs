module Types where
import Data.Typeable (Typeable, typeOf)
type Through a b = a -> Either Error b

data Error
  = FileError FileError
  | TokenisationError TokenisationError
  | ParsingError ParsingError
  | EvaluationError EvaluationError
  deriving (Eq, Show, Typeable)

-- File errors for Main.hs
data FileError
  = NoFilePathProvided
  | FileNotFound String
  deriving (Eq, Show, Typeable)

-- Tokenization errors
data TokenisationError
  = BadChar Char
  | UnrecognizedOperator String
  | UnexpectedEOF
  deriving (Eq, Show, Typeable)

-- Parsing errors
data ParsingError
  = StmtNotFound (Maybe Token)
  | ExprNotFound (Maybe Token)
  | UnparsedInput [Token]
  | SyntaxError String
  | IndentationError String
  | Unexpected (Maybe Token) Token
  deriving (Eq, Show, Typeable)

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
  deriving (Eq, Show, Typeable)

data Token
  = Keyword Keyword
  | Operator Operator
  | Delimiter Delimiter
  -- Since python uses indentation for blocks
  | BlockStart
  | BlockEnd
  | Val Val
  | Ident String -- Can be variable or function name
  deriving (Eq, Show, Typeable)

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
  deriving (Eq, Show, Typeable)

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
  deriving (Eq, Show, Typeable)

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
  deriving (Eq, Show, Typeable)

data Val
  = Int Integer
  | Float Double
  | Str String
  | Bool Bool
  | NoneVal
  | List [Val] 
  deriving (Eq, Show, Typeable)

type Program = Block

type Block = [Stmt]

data Stmt
  = Asgn String Expr
  | While Expr Block
  | Cond Expr Block Block
  | ExprStmt Expr
  | Ret Expr
  deriving (Eq, Show, Typeable)

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
  | NotExp Expr
  | Hat Expr Expr 
  | Tilde Expr 
  | LessThan Expr Expr 
  | GreaterThan Expr Expr 
  | LTEq Expr Expr 
  | GTEq Expr Expr 
  | Eq Expr Expr 
  | NotEq Expr Expr 
  | Identifier String
  | FunctionCall String Expr
  deriving (Eq, Show, Typeable)


showType :: Typeable a => a -> String 
showType x = show (typeOf x)