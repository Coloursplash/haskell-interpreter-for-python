{-# OPTIONS_GHC -Wno-unused-top-binds #-}
module Evaluator (evaluate) where
import Data.Maybe (fromJust)
import Types
import Data.List (genericReplicate)

type VarList = [(String,Val)]

-- | Evaluates the AST (returns a string for now)
evaluate :: Through Block VarList
evaluate = evalBlock []

evalBlock :: VarList -> Through Block VarList
evalBlock vars [] = Right vars
evalBlock vars (s:b) = do 
    vars' <- evalStmt vars s 
    evalBlock vars' b

evalStmt :: VarList -> Through Stmt VarList
evalStmt vars (Asgn str e) = do 
    vars' <- evalExpr vars e 
    Right $ update str vars' vars
evalStmt vars stmt@(While e b) = do 
    val <- evalExpr vars e
    case val of 
        FalseVal -> Right vars 
        TrueVal ->  do 
            vars' <- evalBlock vars b
            evalStmt vars' stmt
        x -> Left (EvaluationError $ TypeError ("Expected type Boolean but got " ++ showType x))
evalStmt vars (Cond e b1 b2) = do 
    val <- evalExpr vars e 
    case val of 
        TrueVal -> do
            vars' <- evalBlock vars b1 
            Right vars' 
        FalseVal -> do 
            vars' <- evalBlock vars b2 
            Right vars' 
evalStmt vars (ExprStmt e) = do 
    val <- evalExpr vars e 
    Right vars
evalStmt vars (Ret e) = undefined 


evalExpr :: VarList -> Through Expr Val
evalExpr vars (ValExp v) = Right v
evalExpr vars (Identifier name) = do 
    case lookup name vars of 
        Just val -> Right val 
        Nothing  -> Left (EvaluationError (NameError $ "Variable '" ++ name ++ "' is not defined."))
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
evalExpr vars (IntDiv e1 e2) = undefined
evalExpr vars (Mod e1 e2) = undefined
evalExpr vars (Pow e1 e2) = undefined
evalExpr vars (At e1 e2) = undefined
evalExpr vars (ShiftL e1 e2) = undefined
evalExpr vars (ShiftR e1 e2) = undefined
evalExpr vars (AndExp e1 e2) = undefined
evalExpr vars (Pipe e1 e2) = undefined
evalExpr vars (Hat e1 e2) = undefined
evalExpr vars (Tilde e) = undefined
evalExpr vars (Assign e1 e2) = undefined
evalExpr vars (LessThan e1 e2) = undefined
evalExpr vars (GreaterThan e1 e2) = undefined
evalExpr vars (LTEq e1 e2) = undefined
evalExpr vars (GTEq e1 e2) = undefined
evalExpr vars (Eq e1 e2) = undefined
evalExpr vars (NotEq e1 e2) = undefined

addVals :: Val -> Val -> Either Error Val
addVals (Int x) (Int y)     = Right $ Int (x + y)
addVals (Float x) (Float y) = Right $ Float (x + y)
addVals (Int x) (Float y)   = Right $ Float (fromIntegral x + y)
addVals (Float x) (Int y)   = Right $ Float (x + fromIntegral y)
addVals (Str x) (Str y)     = Right $ Str (x ++ y)
addVals (List xs) (List ys) = Right $ List (xs ++ ys)
addVals x y = Left  $ EvaluationError $ TypeError $ "Addition is not supported between types " ++ showType x ++ " and " ++ showType y ++ "."

subVals :: Val -> Val -> Either Error Val
subVals (Int x) (Int y)     = Right $ Int (x - y)
subVals (Float x) (Float y) = Right $ Float (x - y)
subVals (Int x) (Float y)   = Right $ Float (fromIntegral x - y)
subVals (Float x) (Int y)   = Right $ Float (x - fromIntegral y)
subVals x y = Left  $ EvaluationError $ TypeError $ "Subtraction is not supported between types " ++ showType x ++ " and " ++ showType y ++ "."

mulVals :: Val -> Val -> Either Error Val
mulVals (Int x) (Int y)     = Right $ Int (x * y)
mulVals (Float x) (Float y) = Right $ Float (x * y)
mulVals (Int x) (Float y)   = Right $ Float (fromIntegral x * y)
mulVals (Float x) (Int y)   = Right $ Float (x * fromIntegral y)
mulVals (List xs) (Int y)   = Right $ List (concat $ genericReplicate y xs)
mulVals x y = Left  $ EvaluationError $ TypeError $ "Multiplication is not supported between types " ++ showType x ++ " and " ++ showType y ++ "."

divVals :: Val -> Val -> Either Error Val
divVals (Int x) (Int y)     
    | x `mod` y == 0 = Right $ Int (x `div` y)
    | otherwise      = Right $ Float (fromIntegral x / fromIntegral y)
divVals (Float x) (Float y) = Right $ Float (x / y)
divVals (Int x) (Float y)   = Right $ Float (fromIntegral x / y)
divVals (Float x) (Int y)   = Right $ Float (x / fromIntegral y)
divVals x y = Left  $ EvaluationError $ TypeError $ "Division is not supported between types " ++ showType x ++ " and " ++ showType y ++ "."


-- technically should have type VarList -> Through Block [Val]
-- however trying to get it working for simple programs first before dealing 
-- with more complex things like that
evalFunc :: VarList -> Through Block Val
evalFunc vars b = undefined

update :: String -> Val -> VarList -> VarList
update str val vars = (str,val) : [(name,val') | (name,val') <- vars, name /= str]
