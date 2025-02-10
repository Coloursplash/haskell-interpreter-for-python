module Evaluator (evaluate) where

import Types

-- | Evaluates the AST (returns a string for now)
evaluate :: Through Block (IO ())
evaluate ast = Right (putStrLn $ "Result: " ++ show ast)