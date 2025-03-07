module StdLib (callStdLib) where

import Control.Monad.Trans.Except (runExceptT)
import Data.List (intercalate, isPrefixOf)
import GHC.Generics (Datatype (moduleName, packageName))
import GHC.IO (unsafePerformIO)
import Parser (parseAtom)
import System.Exit (ExitCode (..))
import System.IO.Unsafe (unsafePerformIO)
import System.Process (readProcessWithExitCode)
import System.Timeout (timeout)
import Tokeniser (tokenise)
import Types

callStdLib :: String -> String -> String -> Through [Val] Expr
callStdLib packageName moduleName funcName args = unsafePerformIO $ do
  let pythonCmd = "python3"
  let formattedArgs = intercalate "," (map show args)
  let importStr =
        if packageName == ""
          then "import " ++ moduleName
          else "from " ++ packageName ++ " import " ++ moduleName
  let pythonCode = getPythonCode importStr funcName formattedArgs
  result <- timeout 2000000 (runPython pythonCmd pythonCode) -- 2-second timeout
  return $ case result of
    Just (Right output) -> parsePythonOutput output funcName
    Just (Left err) -> runtimeError err
    Nothing -> Left $ EvaluationError (Timeout "Python standard library execution timed out.")

runPython :: String -> String -> IO (Either String String)
runPython cmd code = do
  (exitCode, stdout, stderr) <- readProcessWithExitCode cmd ["-c", code] ""
  return $ case exitCode of
    ExitSuccess -> Right (trim stdout)
    ExitFailure _ -> Left (trim stderr)

parsePythonOutput :: String -> String -> Either Error Expr 
parsePythonOutput "CODE-5698308319-9160947241" funcName = Left $ EvaluationError (PythonStdLibNonPrimitive (funcName ++ "()"))
parsePythonOutput s _ = do
  case tokenise s >>= parseAtom of
    Left _ -> runtimeError "(HIPY) Could not parse returned value"
    Right (_, v) -> Right v

getPythonCode :: String -> String -> String -> String
getPythonCode importStr funcName args =
  -- | Uses EXTREMELY specific code string to check later for non-primitive error
  -- i.e. type returned is an object even after str() call so cannot be handled by HIPY
  -- | NOTE: isinstance does not check for tuple since many custom datatypes have a tuple interface e.g., time.struct_time
  -- and even if they were returned as tuples, code that requires accessing them as time.tm_year would break
 importStr
    ++ "; x = "
    ++ funcName
    ++ "("
    ++ args
    ++ ")"
    ++ "; x = x if (isinstance(x, (int,float,complex,list,range,dict,set,frozenset,bool,bytes,bytearray,memoryview,type(None))))"
    -- ++ "else repr(str(x))"
    ++ " else 'CODE-5698308319-9160947241' if (repr(x) == str(x)) else '\"' + str(x) + '\"'"
    ++ "; print(x)"

runtimeError :: Through String a
runtimeError msg = Left $ EvaluationError (PythonStdLibRuntimeError "Error occurred during Python standard library call" msg)

trim :: String -> String
trim = unwords . words