module AST (Expr) where

-- Data types to represent expressions in the AST
data Expr
  = Num Int          -- Represents a number (e.g., 5)
  | Add Expr Expr    -- Represents an addition expression (e.g., x + y)
  | Sub Expr Expr    -- Represents a subtraction expression (e.g., x - y)
  | Mul Expr Expr    -- Represents a multiplication expression (e.g., x * y)
  | Div Expr Expr    -- Represents a division expression (e.g., x / y)
  | Identifier String -- Represents a variable (e.g., "x")
  deriving (Show, Eq)