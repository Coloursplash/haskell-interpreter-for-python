module Main (main) where

import Control.Exception (ErrorCall (ErrorCall), IOException, try)
import Data.Char (isSpace)
import Data.List (dropWhileEnd)
import Evaluator (evaluate)
import Parser (parse)
import System.Environment (getArgs)
import System.IO.Error
import Tokeniser (tokenise)
import Types

-- | Main function to run the interpreter
runInterpreter :: Through String (IO ())
runInterpreter input = do
  tokens <- tokenise input
  ast <- parse tokens
  -- returns varList for now
  varList <- evaluate ast
  return $ print varList

getFileContents :: [String] -> IO (Either Error String)
getFileContents [] = return $ Left (FileError NoFilePathProvided)
getFileContents (path : _) = do
  result <- try (readFile path) :: IO (Either IOError String)
  return $
    either
      (const (Left (FileError (FileNotFound path))))
      (Right . dropWhileEnd isSpace)
      result

main :: IO ()
main = do
  args <- getArgs
  fileResult <- getFileContents args
  -- Will print if either functions returns an error, otherwise nothing
  either print (either print id . runInterpreter) fileResult
