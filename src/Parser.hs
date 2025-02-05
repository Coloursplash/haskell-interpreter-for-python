module Parser (parse) where

import Tokeniser (tokenise)

-- | Parse the tokenized input into an AST
parse :: [String] -> [String]
parse tokens = tokens  -- Just return the tokens for now