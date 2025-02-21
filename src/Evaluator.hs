{-# OPTIONS_GHC -Wno-unused-top-binds #-}
module Evaluator (evaluate) where
import Data.Maybe (fromJust)
import Types

type VarList = [(String,Val)]

-- | Evaluates the AST (returns a string for now)
evaluate :: Through Block VarList
evaluate = evalBlock []

evalBlock :: VarList -> Through Block VarList
evalBlock b vars = undefined

evalStmt :: VarList -> Through Stmt VarList
evalStmt stmt vars = undefined 

evalExpr :: VarList -> Through Expr VarList
evalExpr stmt vars = undefined 

update :: String -> Int -> [(String,Int)] -> [(String,Int)]
update str n vars = (str,n) : [(name,val) | (name,val) <- vars, name /= str]

lookUp :: Eq a => a -> [(a,b)] -> b
lookUp = (fromJust .) . lookup
