{-# OPTIONS_GHC -Wno-unused-top-binds #-}
module Evaluator (evaluate) where

import Control.Monad (foldM, join)
import System.IO ( hFlush, stdout )
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except (except, runExceptT, throwE)
import Data.Bits (Bits (xor))
import Data.List (genericReplicate, intercalate)
import Data.Maybe (fromJust)
import Types

-- | Evaluates the AST (returns a string for now)
evaluate :: ThroughIO Block VarList
evaluate = evalBlock []

evalBlock :: VarList -> ThroughIO Block VarList
evalBlock vars [] = return vars
evalBlock vars (stmt:stmts) = do 
  vars' <- evalStmt vars stmt 
  case vars' of 
    retVal@[("%return_val",x)] -> return retVal 
    _ -> evalBlock vars' stmts


evalStmt :: VarList -> ThroughIO Stmt VarList
evalStmt vars (Asgn str e) = do
  vars' <- evalExpr vars e
  return $ update str vars' vars
evalStmt vars stmt@(While e b) = do
  val <- evalExpr vars e
  case val of
    Bool False -> return vars
    Bool True -> do
      vars' <- evalBlock vars b
      evalStmt vars' stmt
    x -> throwE (EvaluationError $ TypeError ("Expected type Boolean but got " ++ showType x))
evalStmt vars (Cond e b1 b2) = do
  val <- evalExpr vars e
  case val of
    Bool True -> do
      evalBlock vars b1
    Bool False -> do
      evalBlock vars b2
evalStmt vars (ExprStmt e) = do
  val <- evalExpr vars e
  return vars
evalStmt vars (Print es) = do
  evalPrint es vars []
  return vars
  where
    evalPrint :: [Expr] -> VarList -> ThroughIO [Expr] ()
    evalPrint [] vars vs = liftIO $ putStrLn $ unwords (reverse $ map valToStr vs)
    evalPrint (e : es) vars vs = do
      val <- evalExpr vars e
      evalPrint es vars (ValExp val : vs)
evalStmt vars (ForLoop str e b) = do
  val <- evalExpr vars e
  case val of
    List xs -> do
      evalForLoop str vars xs b
    x -> throwE $ EvaluationError $ InvalidArgumentsError $ "For loop expected iterable type, but got " ++ showType x ++ "."
evalStmt vars (Ret e) = do
  val <- evalExpr vars e 
  return [("%return_val%",val)]
evalStmt vars (FuncDef str strs b) = return $ update str (Func strs b) vars

evalForLoop :: String -> VarList -> [Expr] -> ThroughIO Block VarList
evalForLoop _ vars [] _       = return vars 
evalForLoop str vars (e:es) b = do 
  val <- evalExpr vars e 
  let vars' = update str val vars in 
    do 
      vars'' <- evalBlock vars' b 
      evalForLoop str vars'' es b 

evalExpr :: VarList -> ThroughIO Expr Val
evalExpr vars (ValExp v) = return v
evalExpr vars (Identifier name) = do
  case lookup name vars of
    Just val -> return val
    Nothing -> throwE (EvaluationError (NameError $ "Variable '" ++ name ++ "' is not defined."))
-- we need to find a way to rewrite this without unsafe perform IO
-- that might require us to change the entire function to ThroughIO
evalExpr vars (Input es) = do
  evalInput es vars 
  where 
    evalInput es vars = do 
      vals <- mapM (evalExpr vars) es
      -- no matter what i tried, i couldnt get putStr to print before getLine
      liftIO $ putStr $ unwords (reverse $ map show vals) 
      liftIO $ hFlush stdout
      inp <- liftIO getLine 
      return $ Str inp

-- hard coded the 'int' and 'str' functions 
-- this could probably 
evalExpr vars (FunctionCall "int" (e:es))
  | null es = do
    val <- evalExpr vars e
    case val of
      (Str s) -> return $ Int $ read s
      x -> throwE $ EvaluationError $ InvalidOperationError ("Cannot perform function 'int' on type '" ++ showType x ++ "'")
  | otherwise = throwE $ EvaluationError $ InvalidArgumentsError ("Function 'int' takes one argument, but " ++ show (length es + 1) ++ " were provided.")
evalExpr vars (FunctionCall "str" (e:es))
  | null es = do
    val <- evalExpr vars e
    return $ Str $ show val
  | otherwise = throwE $ EvaluationError $ InvalidArgumentsError ("Function 'str' takes one argument, but " ++ show (length es + 1) ++ " were provided.")
-- this would be easy to write in simple python so later on for simplicity, this could be treated like a normal 
-- function, but would be pre-computed/parsed
evalExpr vars (FunctionCall "range" es)
  | len > 0 && len <= 3 = do
    let nums = mapM (evalExpr vars) es in
      do
        nums' <- nums
        case nums' of
          [Int stop] -> return $ List [ValExp $ Int x | x <- [0..stop-1]]
          [Int start, Int stop] -> if stop > start
            then return $ List [ValExp $ Int x | x <- [start..stop-1]]
            else throwE $ EvaluationError $ InvalidArgumentsError "In function 'range' the second argument should always be greater than the first"
          [Int start, Int stop, Int step] -> return $ List [ValExp $ Int x | x <- [start,start+step..stop-signum step]]
  | otherwise = throwE $ EvaluationError $ InvalidArgumentsError ("Function 'range' takes 1-3 arguments, but " ++ show len ++ " were provided.")
  where
    len = length es
evalExpr vars (FunctionCall s es) = do 
  let func = lookup s vars
  case func of 
    Just (Func strs b) -> do 
      vals <- mapM (evalExpr vars) es
      let vars' = (s,Func strs b) : zip strs vals
      evalFunc vars' b
    Just x -> throwE $ EvaluationError $ InvalidOperationError "Cannot invoke non function"
    Nothing -> throwE $ EvaluationError $ InvalidOperationError $ s ++ " is not defined."
evalExpr vars (Add e1 e2) = do
  val1 <- evalExpr vars e1
  val2 <- evalExpr vars e2
  except $ addVals val1 val2
evalExpr vars (Sub e1 e2) = do
  val1 <- evalExpr vars e1
  val2 <- evalExpr vars e2
  except $ subVals val1 val2
evalExpr vars (Mul e1 e2) = do
  val1 <- evalExpr vars e1
  val2 <- evalExpr vars e2
  except $ mulVals val1 val2
evalExpr vars (Div e1 e2) = do
  val1 <- evalExpr vars e1
  val2 <- evalExpr vars e2
  except $ divVals val1 val2
evalExpr vars (IntDiv e1 e2) = do
  val1 <- evalExpr vars e1
  val2 <- evalExpr vars e2
  except $ intDivVals val1 val2
evalExpr vars (Mod e1 e2) = do
  val1 <- evalExpr vars e1
  val2 <- evalExpr vars e2
  except $ modVals val1 val2
evalExpr vars (Pow e1 e2) = do
  val1 <- evalExpr vars e1
  val2 <- evalExpr vars e2
  except $ powVals val1 val2
evalExpr vars (At e1 e2) = do
  val1 <- evalExpr vars e1
  val2 <- evalExpr vars e2
  except $ atVals val1 val2
evalExpr vars (ShiftL e1 e2) = do
  val1 <- evalExpr vars e1
  val2 <- evalExpr vars e2
  except $ shiftLVals val1 val2
evalExpr vars (ShiftR e1 e2) = do
  val1 <- evalExpr vars e1
  val2 <- evalExpr vars e2
  except $ shiftRVals val1 val2
evalExpr vars (AndExp e1 e2) = do
  val1 <- evalExpr vars e1
  val2 <- evalExpr vars e2
  except $ andVals val1 val2
evalExpr vars (Pipe e1 e2) = do
  val1 <- evalExpr vars e1
  val2 <- evalExpr vars e2
  except $ pipeVals val1 val2
evalExpr vars (NotExp e1) = do
  val <- evalExpr vars e1
  except $ notVal val
evalExpr vars (Hat e1 e2) = do
  val1 <- evalExpr vars e1
  val2 <- evalExpr vars e2
  except $ hatVals val1 val2
evalExpr vars (Tilde e) = do
  val <- evalExpr vars e
  except $ tildeVal val
evalExpr vars (LessThan e1 e2) = do
  val1 <- evalExpr vars e1
  val2 <- evalExpr vars e2
  except $ lessThanVals val1 val2
evalExpr vars (GreaterThan e1 e2) = do
  val1 <- evalExpr vars e1
  val2 <- evalExpr vars e2
  except $ greaterThanVals val1 val2
evalExpr vars (LTEq e1 e2) = do
  val1 <- evalExpr vars e1
  val2 <- evalExpr vars e2
  except $ ltEqVals val1 val2
evalExpr vars (GTEq e1 e2) = do
  val1 <- evalExpr vars e1
  val2 <- evalExpr vars e2
  except $ gtEqVals val1 val2
evalExpr vars (Eq e1 e2) = do
  val1 <- evalExpr vars e1
  val2 <- evalExpr vars e2
  except $ eqVals val1 val2
evalExpr vars (NotEq e1 e2) = do
  val1 <- evalExpr vars e1
  val2 <- evalExpr vars e2
  except $ notEqVals val1 val2

addVals :: Val -> Val -> Either Error Val
addVals (Int x) (Int y) = Right $ Int (x + y)
addVals (Float x) (Float y) = Right $ Float (x + y)
addVals (Int x) (Float y) = Right $ Float (fromIntegral x + y)
addVals (Float x) (Int y) = Right $ Float (x + fromIntegral y)
addVals (Str x) (Str y) = Right $ Str (x ++ y)
addVals (List xs) (List ys) = Right $ List (xs ++ ys)
addVals x y = Left $ EvaluationError $ TypeError $ "Addition is not supported between types " ++ showValType x ++ " and " ++ showValType y ++ "."

subVals :: Val -> Val -> Either Error Val
subVals (Int x) (Int y) = Right $ Int (x - y)
subVals (Float x) (Float y) = Right $ Float (x - y)
subVals (Int x) (Float y) = Right $ Float (fromIntegral x - y)
subVals (Float x) (Int y) = Right $ Float (x - fromIntegral y)
subVals x y = Left $ EvaluationError $ TypeError $ "Subtraction is not supported between types " ++ showValType x ++ " and " ++ showValType y ++ "."

mulVals :: Val -> Val -> Either Error Val
mulVals (Int x) (Int y) = Right $ Int (x * y)
mulVals (Float x) (Float y) = Right $ Float (x * y)
mulVals (Int x) (Float y) = Right $ Float (fromIntegral x * y)
mulVals (Float x) (Int y) = Right $ Float (x * fromIntegral y)
mulVals (List xs) (Int y) = Right $ List (concat $ genericReplicate y xs)
mulVals (Int x) (List ys) = Right $ List (concat $ genericReplicate x ys)
mulVals (Int x) (Str ys) = Right $ Str (concat $ genericReplicate x ys)
mulVals (Str xs) (Int y) = Right $ Str (concat $ genericReplicate y xs)
mulVals x y = Left $ EvaluationError $ TypeError $ "Multiplication is not supported between types " ++ showValType x ++ " and " ++ showValType y ++ "."

divVals :: Val -> Val -> Either Error Val
divVals (Int x) (Int y)
  | x `mod` y == 0 = Right $ Int (x `div` y)
  | otherwise = Right $ Float (fromIntegral x / fromIntegral y)
divVals (Float x) (Float y) = Right $ Float (x / y)
divVals (Int x) (Float y) = Right $ Float (fromIntegral x / y)
divVals (Float x) (Int y) = Right $ Float (x / fromIntegral y)
divVals x y = Left $ EvaluationError $ TypeError $ "Division is not supported between types " ++ showValType x ++ " and " ++ showValType y ++ "."

intDivVals :: Val -> Val -> Either Error Val
intDivVals (Int x) (Int y) = Right $ Int (x `div` y)
intDivVals (Float x) (Int y) = Right $ Int (truncate x `div` y)
intDivVals (Int x) (Float y) = Right $ Int $ truncate (fromIntegral x / y)
intDivVals (Float x) (Float y) = Right $ Int $ truncate (x / y)
intDivVals x y = Left $ EvaluationError $ TypeError $ "Integer Division is not supported between types " ++ showValType x ++ " and " ++ showValType y ++ "."

modVals :: Val -> Val -> Either Error Val
modVals (Int x) (Int y) = Right $ Int (x `mod` y)
modVals (Float x) (Int y) = Right $ Float $ doubleMod x (fromIntegral y)
modVals (Int x) (Float y) = Right $ Float $ doubleMod (fromIntegral x) y
modVals (Float x) (Float y) = Right $ Float $ doubleMod x y
modVals x y = Left $ EvaluationError $ TypeError $ "mod is not supported between types " ++ showValType x ++ " and " ++ showValType y ++ "."

doubleMod :: Double -> Double -> Double
doubleMod x y = x - (fromIntegral (floor (x / y)) * y)

powVals :: Val -> Val -> Either Error Val
powVals (Int x) (Int y) = Right $ Int (x ^ y)
powVals (Float x) (Int y) = Right $ Float (x ** fromIntegral y)
powVals (Int x) (Float y) = Right $ Float (fromIntegral x ** y)
powVals (Float x) (Float y) = Right $ Float (x ** y)
powVals x y = Left $ EvaluationError $ TypeError $ "Exponentiation is not supported between types " ++ showValType x ++ " and " ++ showValType y ++ "."

atVals :: Val -> Val -> Either Error Val
atVals = undefined

shiftLVals :: Val -> Val -> Either Error Val
shiftLVals = undefined

shiftRVals :: Val -> Val -> Either Error Val
shiftRVals = undefined

andVals :: Val -> Val -> Either Error Val
andVals (Bool x) (Bool y) = Right $ Bool (x && y)
andVals x y = Left $ EvaluationError $ TypeError $ "Logical AND is not supported between types " ++ showValType x ++ " and " ++ showValType y ++ "."

pipeVals :: Val -> Val -> Either Error Val
pipeVals (Bool x) (Bool y) = Right $ Bool (x || y)
pipeVals x y = Left $ EvaluationError $ TypeError $ "Logical OR is not supported between types " ++ showValType x ++ " and " ++ showValType y ++ "."

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
lessThanVals x y = Left $ EvaluationError $ TypeError $ "Less than comparison is not supported between types " ++ showValType x ++ " and " ++ showValType y ++ "."

greaterThanVals :: Val -> Val -> Either Error Val
greaterThanVals (Int x) (Int y) = Right $ Bool (x > y)
greaterThanVals (Float x) (Float y) = Right $ Bool (x > y)
greaterThanVals (Int x) (Float y) = Right $ Bool (fromIntegral x > y)
greaterThanVals (Float x) (Int y) = Right $ Bool (x > fromIntegral y)
greaterThanVals x y = Left $ EvaluationError $ TypeError $ "Greater than comparison is not supported between types " ++ showValType x ++ " and " ++ showValType y ++ "."

ltEqVals :: Val -> Val -> Either Error Val
ltEqVals (Int x) (Int y) = Right $ Bool (x <= y)
ltEqVals (Float x) (Float y) = Right $ Bool (x <= y)
ltEqVals (Int x) (Float y) = Right $ Bool (fromIntegral x <= y)
ltEqVals (Float x) (Int y) = Right $ Bool (x <= fromIntegral y)
ltEqVals x y = Left $ EvaluationError $ TypeError $ "Less than or equal comparison is not supported between types " ++ showValType x ++ " and " ++ showValType y ++ "."

gtEqVals :: Val -> Val -> Either Error Val
gtEqVals (Int x) (Int y) = Right $ Bool (x >= y)
gtEqVals (Float x) (Float y) = Right $ Bool (x >= y)
gtEqVals (Int x) (Float y) = Right $ Bool (fromIntegral x >= y)
gtEqVals (Float x) (Int y) = Right $ Bool (x >= fromIntegral y)
gtEqVals x y = Left $ EvaluationError $ TypeError $ "Greater than or equal comparison is not supported between types " ++ showValType x ++ " and " ++ showValType y ++ "."

eqVals :: Val -> Val -> Either Error Val
eqVals x y = Right $ Bool (x == y)

notEqVals :: Val -> Val -> Either Error Val
notEqVals x y = Right $ Bool (x /= y)

-- technically should have type VarList -> Through Block [Val]
-- however trying to get it working for simple programs first before dealing
-- with more complex things like that
evalFunc :: VarList -> ThroughIO Block Val
evalFunc [("%return_val%",x)] [] = return x
evalFunc vars [] = return NoneVal
evalFunc vars (Ret e : stmts) = evalExpr vars e
evalFunc vars (s : stmts) = do 
  vars' <- evalStmt vars s
  evalFunc vars' stmts     

update :: String -> Val -> VarList -> VarList
update str val vars = (str, val) : [(name, val') | (name, val') <- vars, name /= str]
