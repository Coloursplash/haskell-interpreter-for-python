module Types (Expr) where

type Tokenizer a = String -> Either Error ([Token], a)
type Parser a = [Token] -> Either Error ([Token], a)
type Evaluator a = a -> Either Error a

data Error
    = TokenisationError TokenisationError
    | ParsingError ParsingError
    | EvaluationError EvaluationError
    deriving (Eq, Show)

-- Tokenization errors
data TokenisationError
    = BadChar Char
    | UnrecognizedToken String
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

type Program = Block

type Block = [Stmt]

data Stmt = Asgn String Expr
          | While Expr Block
          deriving (Eq, Show)

data Expr = Add Expr Expr
          | Mul Expr Expr
          | Val Int
          | Var String
          deriving (Eq, Show)

data Token = Eq | Plus | Minus | Times | LParen | RParen | LBrace | RBrace
           | Semi | Nat Int | Ident String | WhileTok
           deriving (Eq, Show)

data Expr
  = Num Int
  | Add Expr Expr
  | Sub Expr Expr
  | Mul Expr Expr
  | Div Expr Expr
  | Identifier String
  deriving (Show, Eq)