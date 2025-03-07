{-# LANGUAGE InstanceSigs #-}

module Types where

import Control.Monad.Trans.Except (ExceptT)
import Data.List (intercalate)
import Data.Typeable (Typeable, typeOf)

type Through a b = a -> Either Error b

-- Designed this to allow for print statements whilst running programs
-- this implementation meant fewer changes to the original code and less
-- need for 'case ... of' expressions than using a -> IO (Either Error b)
-- might have required. Still not 100% sure what the best option will be
-- going forward so this might be changed later.
type ThroughIO a b = a -> ExceptT Error IO b

type VarList = [(String, Val)]

data Error
  = FileError FileError
  | TokenisationError TokenisationError
  | ParsingError ParsingError
  | EvaluationError EvaluationError
  deriving (Eq, Show, Typeable)

-- File errors for Main.hs
data FileError
  = NoFilePathProvided
  | FileNotFound String -- UNUSED
  deriving (Eq, Show, Typeable)

-- Tokenization errors
data TokenisationError
  = BadChar Char
  | UnrecognizedOperator String
  | UnexpectedEOF
  deriving (Eq, Show, Typeable)

-- Parsing errors
data ParsingError
  = StmtNotFound (Maybe Token) -- UNUSED
  | ExprNotFound (Maybe Token)
  | UnparsedInput [Token] -- UNUSED
  | SyntaxError String -- UNUSED
  | IndentationError String
  | Unexpected (Maybe Token) Token
  | UnknownError
  | InvalidTypeError String
  deriving (Eq, Show, Typeable)

-- Evaluation errors
data EvaluationError
  = TypeError String
  | NameError String
  | AttributeError String -- UNUSED
  | IndexError String
  | KeyError String -- UNUSED
  | ZeroDivisionError String -- UNUSED
  | RuntimeError String -- UNUSED
  | PythonStdLibRuntimeError String String
  | PythonStdLibNonPrimitive String
  | IOError String -- UNUSED
  | InvalidOperationError String
  | InvalidArgumentsError String
  | Timeout String
  deriving (Eq, Show, Typeable)

data Token
  = Keyword Keyword
  | Operator Operator
  | Delimiter Delimiter
  | BlockStart
  | BlockEnd
  | Val Val
  | Ident String -- Can be variable or function name (this gets worked out during parsing)
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
  | ImportTok
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
  | List [Expr]
  | Dict [(Expr, Expr)]
  | Func [String] Block
  | Module String String
  deriving (Eq, Typeable)

instance Show Val where
  show :: Val -> String
  show (Int i) = show i
  show (Float f) = show f
  show (Str s) = '"' : s ++ "\""
  show (Bool b) = show b
  show NoneVal = "None"
  show (List vs) = "[" ++ intercalate ", " (map valToStr vs) ++ "]"
  show (Dict ps) = "{" ++ intercalate ", " (map (\(k, v) -> show k ++ ": " ++ show v) ps) ++ "}"
  show (Func ss b) = "(" ++ intercalate ", " ss ++ ") -> " ++ show b
  show (Module pac mod) = "(Module) " ++ pac ++ " : " ++ mod -- Unsure how to format this
type Program = Block

type Block = [Stmt]

data Stmt
  = Asgn String Expr
  | While Expr Block
  | Cond Expr Block Block
  | FuncDef String [String] Block
  | ForLoop String Expr Block -- This would be interpreted as for 'string' in 'expr' do 'block'
  | ExprStmt Expr
  | Ret Expr
  | Print [Expr]
  | Import String String
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
  | Input [Expr]
  | MethodCall Expr String [Expr] -- Expr is needed here for cases where the object is not assigned to a variable
  | FunctionCall String [Expr]
  deriving (Eq, Show, Typeable)

valToStr :: Expr -> String
valToStr (ValExp val) = show val
valToStr (Identifier x) = x
valToStr x = show x

showType :: (Typeable a) => a -> String
showType x = show (typeOf x)

showValType :: Val -> String
showValType (Int _) = "Int"
showValType (Float _) = "Float"
showValType (Str _) = "Str"
showValType (List _) = "List"
showValType (Bool _) = "Bool"
showValType NoneVal = "None"