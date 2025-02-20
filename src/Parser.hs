{-# OPTIONS_GHC -Wno-unused-top-binds #-}
module Parser (parse) where

import Tokeniser (tokenise)
import Types

-- | Parse the tokenised input into an AST
parse :: Through [Token] Block
parse [] = Right []
parse toks = do
  (toks', b) <- parseBlock toks
  if null toks' then Right b else Left (ParsingError (UnparsedInput toks'))

parseBlock :: Through [Token] ([Token], Block)
parseBlock toks = parseBlock' [] (BlockStart : toks)
  where
    parseBlock' :: Block -> Through [Token] ([Token], Block)
    parseBlock' = undefined

parseStmt :: Through [Token] ([Token], Stmt)
parseStmt = undefined

parseExpr :: Through [Token] ([Token], Expr)
parseExpr = undefined

parseComparison :: Through [Token] ([Token], Expr)
parseComparison = undefined

parseTerm :: Through [Token] ([Token], Expr)
parseTerm = undefined

parseFactor :: Through [Token] ([Token], Expr)
parseFactor = undefined

parseExponent :: Through [Token] ([Token], Expr)
parseExponent = undefined

-- handles lowest level of expressions - strings, ints etc
parseAtom :: Through [Token] ([Token], Expr)
parseAtom tks = undefined