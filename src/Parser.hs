module Parser (parse) where

import Tokenizer (tokenize)

-- | Parse the tokenized input into an AST
parse :: [String] -> [String]
parse tokens = tokens  -- Just return the tokens for now