module Tokeniser (tokenise) where

-- | Tokenizes the input string into a list of tokens
tokenise :: String -> [String]
tokenise input = words input  -- A simple tokenizer splitting by spaces for now