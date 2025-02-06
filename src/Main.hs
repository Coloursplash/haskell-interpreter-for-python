module Main (main) where

import Control.Exception (IOException, try)
import Data.Char (isSpace)
import Data.List (dropWhileEnd)
import Evaluator (evaluate)
import Parser (parse)
import System.Environment (getArgs)
import Tokeniser (tokenise)
import Types
import Control.Exception (ErrorCall(ErrorCall))

-- | Main function to run the interpreter
runInterpreter :: String -> IO ()
runInterpreter input = do
  let tokens = tokenise input
  let ast = parse tokens
  let result = evaluate ast
  putStrLn $ "Result: " ++ show result


getFileContents :: FileReader String
getFileContents "" = return $ Left NoFilePathProvided
getFileContents filePath = do
  result <- try (readFile filePath) :: IO (Either IOException String)
  return $ case result of
    Left _  -> Left (FileNotFound filePath)
    Right contents -> Right (dropWhileEnd isSpace contents)

main :: IO ()
main = do
  args <- getArgs
  result <- getFileContents (if null args then "" else head args)
  print result
  