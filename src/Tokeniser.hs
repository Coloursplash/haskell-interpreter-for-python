module Tokeniser (tokenise) where

-- | Tokenises the input string into a list of tokens
tokenise :: String -> [String]
tokenise input = words input  -- A simple tokeniser splitting by spaces for now