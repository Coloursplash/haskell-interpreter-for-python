module Evaluator (evaluate) where

-- | Evaluates the AST (returns a string for now)
evaluate :: [String] -> String
evaluate ast = unwords ast  -- Return the tokens as a simple evaluation