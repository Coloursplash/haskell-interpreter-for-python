{-# OPTIONS_GHC -Wno-unused-top-binds #-}
module Evaluator (evaluate) where
import Data.Maybe (fromJust)
import Types

type VarList = [(String,Val)]

-- | Evaluates the AST (returns a string for now)
evaluate :: Through Block VarList
evaluate = evalBlock []

evalBlock :: VarList -> Through Block VarList
evalBlock vars b = undefined

evalStmt :: VarList -> Through Stmt VarList
evalStmt vars stmt = undefined 

evalExpr :: VarList -> Through Expr VarList
evalExpr vars (Add e1 e2) = undefined 

evalFunc :: VarList -> Through Block VarList
evalFunc vars b = undefined

update :: String -> Val -> VarList -> VarList
update str val vars = (str,val) : [(name,val') | (name,val') <- vars, name /= str]

lookUp :: Eq a => a -> [(a,b)] -> b
lookUp = (fromJust .) . lookup
