{-# LANGUAGE OverloadedStrings #-}

module StdLib (callStdLib, initPython) where

import qualified CPython as Py
import qualified CPython.Types as Py
import qualified CPython.Types.Module as Py
import qualified CPython.Types.Function as Py
import qualified CPython.Types.Object as Py
import qualified CPython.Types.Tuple as Py
import qualified CPython.Types.String as Py
import qualified CPython.Types.Int as Py
import qualified CPython.Types.Float as Py
import Control.Monad (void)
import Types

initStdLib :: IO ()
initStdLib = Py.initialize

finishStdLib :: IO ()
finishStdLib = Py.finalize

convertPyToVal :: Py.Object -> IO Val
convertPyToVal obj
    | Just pyInt <- Py.cast obj = Py.fromInt pyInt >>= \n -> return (VInt (fromIntegral n))
    | Just pyFloat <- Py.cast obj = Py.fromFloat pyFloat >>= \f -> return (VFloat f)
    | Just pyStr <- Py.cast obj = Py.fromUnicode pyStr >>= \s -> return (VStr s)
    | otherwise = return VNone

convertValToPy :: Val -> IO Py.Object
convertValToPy (VInt n) = Py.toInt (fromIntegral n)
convertValToPy (VFloat f) = Py.toFloat f
convertValToPy (VStr s) = Py.toUnicode s
convertValToPy _ = Py.toUnicode ""

-- Call Python standard library functions
callStdLib :: String -> String -> [Val] -> IO Val
callStdLib moduleName funcName args = do
    pyModule <- Py.importModule moduleName
    pyFunc <- Py.getAttribute pyModule =<< Py.toUnicode funcName
    pyArgs <- mapM convertValToPy args
    result <- Py.callObject pyFunc pyArgs
    convertPyToVal result