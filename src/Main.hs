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



getFileContents :: Through [String] (IO String)
getFileContents [] = Left (FileError NoFilePathProvided)
getFileContents (path:_) = Right $ do
    result <- try (readFile path) :: IO (Either IOException String)
    return $ either (const (Left (FileError (FileNotFound path))))
                    (Right . dropWhileEnd isSpace)
                    result


main :: IO ()
main = do
    args <- getArgs
    print =<< case getFileContents args of
        Left err -> return (Left err)
        Right io -> io
  