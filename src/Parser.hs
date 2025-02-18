module Parser (parse) where

import Tokeniser (tokenise)
import Types

-- | Parse the tokenised input into an AST
parse :: Through [Token] Block
parse tokens = Right []