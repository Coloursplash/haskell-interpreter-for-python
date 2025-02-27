module Main (main) where

import Control.Exception (ErrorCall (ErrorCall), IOException, try)
import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Trans.Except (ExceptT, except, runExceptT, throwE)
import Data.Char (isSpace, toLower)
import Data.List (dropWhileEnd)
import Evaluator (evaluate)
import Parser (parse)
import StdLib (initStdLib, finishStdLib)
import System.Environment (getArgs)
import System.IO.Error
import Tokeniser (tokenise)
import Types

-- | Main function to run the interpreter
runInterpreter :: Bool -> ThroughIO String ()
runInterpreter debug input = do
  tokens <- except $ tokenise input
  when debug (liftIO $ putStrLn "Finished Tokenisation")
  when debug (liftIO $ print tokens)
  ast <- except $ parse tokens
  when debug (liftIO $ print ast)
  when debug (liftIO $ putStrLn "Tokens Parsed\n")
  when debug (liftIO $ putStrLn "Executing...\n--------------")
  varList <- evaluate ast
  when debug (liftIO $ putStrLn "--------------\nExecution Complete")
  when debug (liftIO $ putStrLn "\nAll variables and their values post-execution:")
  when debug (liftIO $ print varList)
  when debug (liftIO $ putStrLn "--------------------------------------------------------------")

getFileContents :: ThroughIO [String] (Either Error (Bool, String))
getFileContents [] = throwE (FileError NoFilePathProvided)
getFileContents (path : debug : _) = do
  result <- liftIO (readFile path)
  case map toLower debug of
    "true" -> return $ Right (True, result)
    _      -> return $ Right (False, result)
getFileContents (path : _) = do
  result <- liftIO (readFile path)
  return $ Right (False, result)

main :: IO ()
main = do
  args <- getArgs

  fileContents <- runExceptT $ getFileContents args

  (debug, contents) <- case fileContents of
    Left err -> error ("File error: " ++ show err)
    Right inner -> case inner of
        Left err -> error ("Inner error: " ++ show err)
        Right f -> return f

  when debug (putStrLn "--------------------------------------------------------------")
  when debug (putStrLn "Starting...\n")
  
  initStdLib
  result <- runExceptT $ runInterpreter debug contents
  finishStdLib

  case result of
    Left err -> print err
    Right _ -> when debug (putStrLn "Interpretation completed successfully")
