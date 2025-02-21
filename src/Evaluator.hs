{-# OPTIONS_GHC -Wno-unused-top-binds #-}
module Evaluator (evaluate) where
import Data.Maybe (fromJust)
import Types

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

 

evalExpr :: VarList -> Through Expr Val
evalExpr vars (Add e1 e2) = undefined 

-- technically should have type VarList -> Through Block [Val]
-- however trying to get it working for simple programs first before dealing 
-- with more complex things like that
evalFunc :: VarList -> Through Block Val
evalFunc vars b = undefined

update :: String -> Val -> VarList -> VarList
update str val vars = (str,val) : [(name,val') | (name,val') <- vars, name /= str]

lookUp :: Eq a => a -> [(a,b)] -> b
lookUp = (fromJust .) . lookup
