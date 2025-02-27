{-# LANGUAGE OverloadedStrings #-}

module StdLib (callStdLib, initStdLib, finishStdLib) where

import qualified CPython as Py
import qualified CPython.Types as Py
import qualified CPython.Types.Module as Py
import qualified CPython.Types.Function as Py
import qualified CPython.Types.Tuple as Py
import qualified CPython.Types.Float as Py
import Control.Monad (void)
import Types

initStdLib :: IO ()
initStdLib = Py.initialize

finishStdLib :: IO ()
finishStdLib = Py.finalize

convertPyToVal :: Py.Type -> IO Val
convertPyToVal _ = undefined

convertValToPy :: Val -> IO Py.Type
convertValToPy _ = undefined

-- Call Python standard library functions
callStdLib :: String -> String -> [Val] -> IO Val
callStdLib moduleName funcName args = do
    -- pyModule <- Py.importModule moduleName
    -- pyFunc <- Py.getAttribute pyModule =<< Py.toUnicode funcName
    -- pyArgs <- map convertValToPy args
    -- result <- Py.callObject pyFunc pyArgs
    -- convertPyToVal result
    return (Int 0)