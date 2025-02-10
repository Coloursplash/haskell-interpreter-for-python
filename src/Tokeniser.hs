module Tokeniser (tokenise) where

import Types

-- | Tokenises the input string into a list of tokens
tokenise :: Through String [String]
tokenise input = Right (words input)