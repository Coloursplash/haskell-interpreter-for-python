module StdLib (callStdLib) where

import Control.Monad.Trans.Except (runExceptT)
import Data.List (intercalate)
import Evaluator (evaluate)
import Parser (parse)
import System.Exit (ExitCode (..))
import System.IO.Unsafe (unsafePerformIO)
import System.Process (readProcessWithExitCode)
import System.Timeout (timeout)
import Tokeniser (tokenise)
import Types
import GHC.IO (unsafePerformIO)

callStdLib :: String -> String -> Through [Val] Val
callStdLib moduleName funcName args = unsafePerformIO $ do
  let pythonCmd = "python3"
  let formattedArgs = intercalate "," (map valToPython args)
  let pythonCode = "import " ++ moduleName ++ "; print('result =', repr(" ++ funcFull ++ "(" ++ formattedArgs ++ ")))"
  result <- timeout 2000000 (runPython pythonCmd pythonCode) -- 2-second timeout
  return $ case result of
    Just (Right output) -> parsePythonOutput output
    Just (Left err) -> Left $ EvaluationError (PythonStdLibRuntimeError err)
    Nothing -> Left $ EvaluationError (Timeout "Python standard library execution timed out.")
  where
    funcFull = moduleName ++ "." ++ funcName

runPython :: String -> String -> IO (Either String String)
runPython cmd code = do
  (exitCode, stdout, stderr) <- readProcessWithExitCode cmd ["-c", code] ""
  return $ case exitCode of
    ExitSuccess -> Right (trim stdout)
    ExitFailure _ -> Left (trim stderr)

valToPython :: Val -> String
valToPython = show

parsePythonOutput :: String -> Either Error Val
parsePythonOutput s = do
  tokenised <- tokenise (replace '\'' '"' s)
  parsed <- parse tokenised
  case unsafePerformIO $ runExceptT $ evaluate parsed of
    Left block -> unknownError
    Right varList -> maybe unknownError Right (lookup "result" varList)

unknownError :: Either Error a
unknownError = Left $ EvaluationError (PythonStdLibRuntimeError "Unknown error occurred during Python standard library call")

replace :: (Eq a) => a -> a -> [a] -> [a]
replace a b = map $ \c -> if c == a then b else c

trim :: String -> String
trim = unwords . words