{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Evaluator (evaluate) where

import Data.List (genericReplicate)
import Data.Maybe (fromJust)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except
import Control.Monad (foldM)
import Types

-- | Evaluates the AST (returns a string for now)
evaluate :: ThroughIO Block VarList
evaluate = evalBlock []

evalBlock :: VarList -> ThroughIO Block VarList
evalBlock = foldM evalStmt

evalStmt :: VarList -> ThroughIO Stmt VarList
evalStmt vars (Asgn str e) = do
  vars' <- except $ evalExpr vars e
  return $ update str vars' vars
evalStmt vars stmt@(While e b) = do
  val <- except $ evalExpr vars e
  case val of
    Bool False -> return vars
    Bool True -> do
      vars' <- evalBlock vars b
      evalStmt vars' stmt
    x -> throwE (EvaluationError $ TypeError ("Expected type Boolean but got " ++ showType x))
evalStmt vars (Cond e b1 b2) = do
  val <- except $ evalExpr vars e
  case val of
    Bool True -> do
      evalBlock vars b1
    Bool False -> do
      evalBlock vars b2
evalStmt vars (ExprStmt e) = do
  val <- except $ evalExpr vars e
  return vars
evalStmt vars (Print e) = do
    val <- except $ evalExpr vars e
    liftIO $ print val
    return vars
evalStmt vars (Ret e) = undefined

evalExpr :: VarList -> Through Expr Val
evalExpr vars (ValExp v) = Right v
evalExpr vars (Identifier name) = do
  case lookup name vars of
    Just val -> Right val
    Nothing -> Left (EvaluationError (NameError $ "Variable '" ++ name ++ "' is not defined."))
evalExpr vars (Add e1 e2) = do
  val1 <- evalExpr vars e1
  val2 <- evalExpr vars e2
  addVals val1 val2
evalExpr vars (Sub e1 e2) = do
  val1 <- evalExpr vars e1
  val2 <- evalExpr vars e2
  subVals val1 val2
evalExpr vars (Mul e1 e2) = do
  val1 <- evalExpr vars e1
  val2 <- evalExpr vars e2
  mulVals val1 val2
evalExpr vars (Div e1 e2) = do
  val1 <- evalExpr vars e1
  val2 <- evalExpr vars e2
  divVals val1 val2
evalExpr vars (IntDiv e1 e2) = do
  val1 <- evalExpr vars e1
  val2 <- evalExpr vars e2
  intDivVals val1 val2
evalExpr vars (Mod e1 e2) = do
  val1 <- evalExpr vars e1
  val2 <- evalExpr vars e2
  modVals val1 val2
evalExpr vars (Pow e1 e2) = do
  val1 <- evalExpr vars e1
  val2 <- evalExpr vars e2
  powVals val1 val2
evalExpr vars (At e1 e2) = do
  val1 <- evalExpr vars e1
  val2 <- evalExpr vars e2
  atVals val1 val2
evalExpr vars (ShiftL e1 e2) = do
  val1 <- evalExpr vars e1
  val2 <- evalExpr vars e2
  shiftLVals val1 val2
evalExpr vars (ShiftR e1 e2) = do
  val1 <- evalExpr vars e1
  val2 <- evalExpr vars e2
  shiftRVals val1 val2
evalExpr vars (AndExp e1 e2) = do
  val1 <- evalExpr vars e1
  val2 <- evalExpr vars e2
  andVals val1 val2
evalExpr vars (Pipe e1 e2) = do
  val1 <- evalExpr vars e1
  val2 <- evalExpr vars e2
  pipeVals val1 val2
evalExpr vars (NotExp e1) = do
  val <- evalExpr vars e1
  notVal val
evalExpr vars (Hat e1 e2) = do
  val1 <- evalExpr vars e1
  val2 <- evalExpr vars e2
  hatVals val1 val2
evalExpr vars (Tilde e) = do
  val <- evalExpr vars e
  tildeVal val
evalExpr vars (LessThan e1 e2) = do
  val1 <- evalExpr vars e1
  val2 <- evalExpr vars e2
  lessThanVals val1 val2
evalExpr vars (GreaterThan e1 e2) = do
  val1 <- evalExpr vars e1
  val2 <- evalExpr vars e2
  greaterThanVals val1 val2
evalExpr vars (LTEq e1 e2) = do
  val1 <- evalExpr vars e1
  val2 <- evalExpr vars e2
  ltEqVals val1 val2
evalExpr vars (GTEq e1 e2) = do
  val1 <- evalExpr vars e1
  val2 <- evalExpr vars e2
  gtEqVals val1 val2
evalExpr vars (Eq e1 e2) = do
  val1 <- evalExpr vars e1
  val2 <- evalExpr vars e2
  eqVals val1 val2
evalExpr vars (NotEq e1 e2) = do
  val1 <- evalExpr vars e1
  val2 <- evalExpr vars e2
  notEqVals val1 val2

addVals :: Val -> Val -> Either Error Val
addVals (Int x) (Int y) = Right $ Int (x + y)
addVals (Float x) (Float y) = Right $ Float (x + y)
addVals (Int x) (Float y) = Right $ Float (fromIntegral x + y)
addVals (Float x) (Int y) = Right $ Float (x + fromIntegral y)
addVals (Str x) (Str y) = Right $ Str (x ++ y)
addVals (List xs) (List ys) = Right $ List (xs ++ ys)
addVals x y = Left $ EvaluationError $ TypeError $ "Addition is not supported between types " ++ showType x ++ " and " ++ showType y ++ "."

subVals :: Val -> Val -> Either Error Val
subVals (Int x) (Int y) = Right $ Int (x - y)
subVals (Float x) (Float y) = Right $ Float (x - y)
subVals (Int x) (Float y) = Right $ Float (fromIntegral x - y)
subVals (Float x) (Int y) = Right $ Float (x - fromIntegral y)
subVals x y = Left $ EvaluationError $ TypeError $ "Subtraction is not supported between types " ++ showType x ++ " and " ++ showType y ++ "."

mulVals :: Val -> Val -> Either Error Val
mulVals (Int x) (Int y) = Right $ Int (x * y)
mulVals (Float x) (Float y) = Right $ Float (x * y)
mulVals (Int x) (Float y) = Right $ Float (fromIntegral x * y)
mulVals (Float x) (Int y) = Right $ Float (x * fromIntegral y)
mulVals (List xs) (Int y) = Right $ List (concat $ genericReplicate y xs)
mulVals x y = Left $ EvaluationError $ TypeError $ "Multiplication is not supported between types " ++ showType x ++ " and " ++ showType y ++ "."

divVals :: Val -> Val -> Either Error Val
divVals (Int x) (Int y)
  | x `mod` y == 0 = Right $ Int (x `div` y)
  | otherwise = Right $ Float (fromIntegral x / fromIntegral y)
divVals (Float x) (Float y) = Right $ Float (x / y)
divVals (Int x) (Float y) = Right $ Float (fromIntegral x / y)
divVals (Float x) (Int y) = Right $ Float (x / fromIntegral y)
divVals x y = Left $ EvaluationError $ TypeError $ "Division is not supported between types " ++ showType x ++ " and " ++ showType y ++ "."

intDivVals :: Val -> Val -> Either Error Val
intDivVals (Int x) (Int y) = Right $ Int (x `div` y)
intDivVals (Float x) (Int y) = Right $ Int (truncate x `div` y)
intDivVals (Int x) (Float y) = Right $ Int $ truncate (fromIntegral x / y)
intDivVals (Float x) (Float y) = Right $ Int $ truncate (x / y)
intDivVals x y = Left $ EvaluationError $ TypeError $ "Integer Division is not supported between types " ++ showType x ++ " and " ++ showType y ++ "."

modVals :: Val -> Val -> Either Error Val
modVals (Int x) (Int y) = Right $ Int (x `mod` y)
modVals (Float x) (Int y) = Right $ Float $ doubleMod x (fromIntegral y)
modVals (Int x) (Float y) = Right $ Float $ doubleMod (fromIntegral x) y
modVals (Float x) (Float y) = Right $ Float $ doubleMod x y
modVals x y = Left $ EvaluationError $ TypeError $ "Integer Division is not supported between types " ++ showType x ++ " and " ++ showType y ++ "."

doubleMod :: Double -> Double -> Double
doubleMod x y = x - (fromIntegral (floor (x / y)) * y)

powVals :: Val -> Val -> Either Error Val
powVals (Int x) (Int y) = Right $ Int (x ^ y)
powVals (Float x) (Int y) = Right $ Float (x ** fromIntegral y)
powVals (Int x) (Float y) = Right $ Float (fromIntegral x ** y)
powVals (Float x) (Float y) = Right $ Float (x ** y)
powVals x y = Left $ EvaluationError $ TypeError $ "Exponentiation is not supported between types " ++ showType x ++ " and " ++ showType y ++ "."

atVals :: Val -> Val -> Either Error Val
atVals = undefined

shiftLVals :: Val -> Val -> Either Error Val
shiftLVals = undefined

shiftRVals :: Val -> Val -> Either Error Val
shiftRVals = undefined

andVals :: Val -> Val -> Either Error Val
andVals (Bool x) (Bool y) = Right $ Bool (x && y)
andVals x y = Left $ EvaluationError $ TypeError $ "Logical AND is not supported between types " ++ showType x ++ " and " ++ showType y ++ "."

pipeVals :: Val -> Val -> Either Error Val
pipeVals (Bool x) (Bool y) = Right $ Bool (x || y)
pipeVals x y = Left $ EvaluationError $ TypeError $ "Logical OR is not supported between types " ++ showType x ++ " and " ++ showType y ++ "."

notVal :: Val -> Either Error Val
notVal (Bool x) = Right $ Bool $ not x

hatVals :: Val -> Val -> Either Error Val
hatVals = undefined

tildeVal :: Val -> Either Error Val
tildeVal = undefined

assignVals :: Val -> Val -> Either Error Val
assignVals = undefined

lessThanVals :: Val -> Val -> Either Error Val
lessThanVals (Int x) (Int y) = Right $ Bool (x < y)
lessThanVals (Float x) (Float y) = Right $ Bool (x < y)
lessThanVals (Int x) (Float y) = Right $ Bool (fromIntegral x < y)
lessThanVals (Float x) (Int y) = Right $ Bool (x < fromIntegral y)
lessThanVals x y = Left $ EvaluationError $ TypeError $ "Less than comparison is not supported between types " ++ showType x ++ " and " ++ showType y ++ "."

greaterThanVals :: Val -> Val -> Either Error Val
greaterThanVals (Int x) (Int y) = Right $ Bool (x > y)
greaterThanVals (Float x) (Float y) = Right $ Bool (x > y)
greaterThanVals (Int x) (Float y) = Right $ Bool (fromIntegral x > y)
greaterThanVals (Float x) (Int y) = Right $ Bool (x > fromIntegral y)
greaterThanVals x y = Left $ EvaluationError $ TypeError $ "Greater than comparison is not supported between types " ++ showType x ++ " and " ++ showType y ++ "."

ltEqVals :: Val -> Val -> Either Error Val
ltEqVals (Int x) (Int y) = Right $ Bool (x <= y)
ltEqVals (Float x) (Float y) = Right $ Bool (x <= y)
ltEqVals (Int x) (Float y) = Right $ Bool (fromIntegral x <= y)
ltEqVals (Float x) (Int y) = Right $ Bool (x <= fromIntegral y)
ltEqVals x y = Left $ EvaluationError $ TypeError $ "Less than or equal comparison is not supported between types " ++ showType x ++ " and " ++ showType y ++ "."

gtEqVals :: Val -> Val -> Either Error Val
gtEqVals (Int x) (Int y) = Right $ Bool (x >= y)
gtEqVals (Float x) (Float y) = Right $ Bool (x >= y)
gtEqVals (Int x) (Float y) = Right $ Bool (fromIntegral x >= y)
gtEqVals (Float x) (Int y) = Right $ Bool (x >= fromIntegral y)
gtEqVals x y = Left $ EvaluationError $ TypeError $ "Greater than or equal comparison is not supported between types " ++ showType x ++ " and " ++ showType y ++ "."

eqVals :: Val -> Val -> Either Error Val
eqVals x y = Right $ Bool (x == y)

notEqVals :: Val -> Val -> Either Error Val
notEqVals x y = Right $ Bool (x /= y)

-- technically should have type VarList -> Through Block [Val]
-- however trying to get it working for simple programs first before dealing
-- with more complex things like that
evalFunc :: VarList -> Through Block Val
evalFunc vars b = undefined

update :: String -> Val -> VarList -> VarList
update str val vars = (str, val) : [(name, val') | (name, val') <- vars, name /= str]
