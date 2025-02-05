module Tokenizer (tokenize) where

-- | Tokenizes the input string into a list of tokens
tokenize :: String -> [String]
tokenize input = words input  -- A simple tokenizer splitting by spaces for now