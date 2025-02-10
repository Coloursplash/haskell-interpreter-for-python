module Parser (parse) where

import Tokeniser (tokenise)
import Types

-- | Parse the tokenised input into an AST
parse :: Through [String] [String]
parse tokens = Right tokens