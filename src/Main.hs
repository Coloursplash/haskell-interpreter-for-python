module Main (main) where

import Control.Exception (ErrorCall (ErrorCall), IOException, try)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Trans.Except (ExceptT, except, runExceptT, throwE)
import Data.Char (isSpace)
import Data.List (dropWhileEnd)
import Evaluator (evaluate)
import Parser (parse)
import System.Environment (getArgs)
import System.IO.Error
import Tokeniser (tokenise)
import Types

-- | Main function to run the interpreter
runInterpreter :: ThroughIO String ()
runInterpreter input = do
  tokens <- except $ tokenise input
  ast <- except $ parse tokens
  -- returns varList for now
  varList <- evaluate ast
  liftIO $ print varList

getFileContents :: ThroughIO [String] (Either Error String)
getFileContents [] = throwE (FileError NoFilePathProvided)
getFileContents (path : _) = do
  result <- liftIO (readFile path)
  return $ Right result

main :: IO ()
main = do
  args <- getArgs
  result <- runExceptT $ do
    fileContents <- getFileContents args
    case fileContents of
      Left _ -> throwE (FileError (FileNotFound (head args)))
      Right contents -> runInterpreter contents

  case result of
    Left err -> print err
    Right _ -> putStrLn "Interpretation completed successfully"
