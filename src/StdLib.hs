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
import GHC.Generics (Datatype(moduleName, packageName))

callStdLib :: String -> String -> String -> Through [Val] Val
callStdLib packageName moduleName funcName args = unsafePerformIO $ do
  let pythonCmd = "python3"
  let formattedArgs = intercalate "," (map show args)
  let pythonCode = getPythonCode packageName moduleName funcName formattedArgs
  result <- timeout 2000000 (runPython pythonCmd pythonCode) -- 2-second timeout
  return $ case result of
    Just (Right output) -> parsePythonOutput output
    Just (Left err) -> runtimeError err
    Nothing -> Left $ EvaluationError (Timeout "Python standard library execution timed out.")

runPython :: String -> String -> IO (Either String String)
runPython cmd code = do
  (exitCode, stdout, stderr) <- readProcessWithExitCode cmd ["-c", code] ""
  return $ case exitCode of
    ExitSuccess -> Right (trim stdout)
    ExitFailure _ -> Left (trim stderr)

parsePythonOutput :: String -> Either Error Val
parsePythonOutput s = do
  parsed <- case tokenise s >>= parse of
    Left err -> runtimeError $ show err
    Right block -> Right block
  case unsafePerformIO $ runExceptT $ evaluate parsed of
    Left _ -> runtimeError "(HIPY) 'evaluate' returned Left block"
    Right varList -> maybe (runtimeError "(HIPY) Result was not found in varList after evaluating") Right (lookup "result" varList)

getPythonCode :: String -> String -> String -> String -> String
getPythonCode packageName moduleName funcName args = importStr ++
  "; x = " ++ funcName ++ "(" ++ args ++ ")" ++
  "; x = x if (isinstance(x, (int,float,complex,list,tuple,range,dict,set,frozenset,bool,bytes,bytearray,memoryview,type(None)))) else repr(str(x))" ++ 
  "; print('result =', x)"
  where
    importStr = if packageName == ""
      then "import " ++ moduleName
      else "from " ++ packageName ++ " import " ++ moduleName

runtimeError :: Through String a
runtimeError msg = Left $ EvaluationError (PythonStdLibRuntimeError "Error occurred during Python standard library call" msg)

trim :: String -> String
trim = unwords . words