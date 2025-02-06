module Parser (parse) where

import Tokeniser (tokenise)

-- | Parse the tokenised input into an AST
parse :: [String] -> [String]
parse tokens = tokens  -- Just return the tokens for now