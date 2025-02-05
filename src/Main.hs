module Main where

import Tokeniser (tokenise)
import Parser (parse)
import Evaluator (evaluate)

-- | Main function to run the interpreter
runInterpreter :: String -> IO ()
runInterpreter input = do
    let tokens = tokenize input
    let ast = parse tokens
    let result = evaluate ast
    putStrLn $ "Result: " ++ show result

main :: IO ()
main = do
    putStrLn "Starting Haskell Python Interpreter"
    runInterpreter "your input here"