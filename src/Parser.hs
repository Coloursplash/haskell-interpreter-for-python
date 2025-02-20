{-# OPTIONS_GHC -Wno-unused-top-binds #-}
module Parser (parse,parseExpr,parseStmt) where

import Tokeniser (tokenise)
import Types

mHead :: [a] -> Maybe a
mHead (x:_) = Just x
mHead _ = Nothing

checkTok :: Token -> Through [Token] [Token]
checkTok tk (tk':toks)  
    | tk == tk' = Right toks
    | otherwise = Left(ParsingError (Unexpected (mHead toks) tk))
    
-- | Parse the tokenised input into an AST
parse :: Through [Token] Block
parse [] = Right []
parse toks = do
  (toks', b) <- parseBlock toks
  b' <- parse toks' 
  Right (b ++ b')

parseBlock :: Through [Token] ([Token], Block)
parseBlock toks = parseBlock' [] (BlockStart : toks)
  where
    parseBlock' :: Block -> Through [Token] ([Token], Block)
    parseBlock' b (BlockStart:toks) = do 
      (toks',stmt) <- parseStmt toks
      parseBlock' (b ++ [stmt]) toks'
    parseBlock' b toks = Right (toks,b)


-- Did not implement If statements/For statements as we havent fully discussed
-- how to handle those 
parseStmt :: Through [Token] ([Token], Stmt)
parseStmt (Ident x : Operator EqOp : toks) = do 
  (toks',expr) <- parseExpr toks
  Right (toks',Asgn x expr)
parseStmt (Ident x : Delimiter EqDelim : toks) = do 
  (toks',expr) <- parseExpr toks
  Right (toks',Asgn x expr)
parseStmt (Keyword WhileTok : toks) = do 
  (toks',expr) <- parseExpr toks
  (toks'',b) <- parseBody toks'
  Right (toks'',While expr b)
  where 
    parseBody (Delimiter LBrace : toks) = do 
      (toks',b) <- parseBlock toks
      toks'' <- checkTok (Delimiter RBrace) toks'
      Right (toks'',b)
    parseBody tks = Left (ParsingError $ Unexpected (mHead tks) (Delimiter LBrace)) 
parseStmt toks = do 
    (toks',expr) <- parseExpr toks
    Right (toks',ExprStmt expr)


parseExpr :: Through [Token] ([Token], Expr)
parseExpr toks = do 
    (toks',expr) <- parseComparison toks 
    case toks' of 
        (Keyword And:toks'') -> do 
            (toks''',expr') <- parseExpr toks'' 
            Right (toks''',AndExp expr expr')
        (Operator AndOp:toks'') -> do 
            (toks''',expr') <- parseExpr toks'' 
            Right (toks''',AndExp expr expr')
        (Keyword or:toks'') -> do 
            (toks''',expr') <- parseExpr toks'' 
            Right (toks''',Pipe expr expr')
        (Operator PipeOp:Operator PipeOp:toks'') -> do 
            (toks''',expr') <- parseExpr toks'' 
            Right (toks''',Pipe expr expr')
        _ -> Right (toks',expr)

parseComparison :: Through [Token] ([Token], Expr)
parseComparison toks = do 
    (toks',expr) <- parseTerm toks 
    parseComparison' expr toks' 
  where 
    parseComparison' :: Expr -> Through [Token] ([Token],Expr)
    parseComparison' e (Operator GreaterThanOp:toks) = do 
        (toks',expr) <- parseTerm toks
        Right (toks',GreaterThan e expr)
    parseComparison' e (Operator LessThanOp:toks) = do 
        (toks',expr) <- parseTerm toks
        Right (toks',LessThan e expr)
    parseComparison' e (Operator LTEqOp:toks) = do 
        (toks',expr) <- parseTerm toks
        Right (toks',LTEq e expr)
    parseComparison' e (Operator GTEqOp:toks) = do 
        (toks',expr) <- parseTerm toks
        Right (toks',GTEq e expr)
    parseComparison' e (Operator EqOp:toks) = do 
        (toks',expr) <- parseTerm toks
        Right (toks',Eq e expr)
    parseComparison' e (Operator NotEqOp:toks) = do 
        (toks',expr) <- parseTerm toks
        Right (toks',NotEq e expr)
    parseComparison' e toks = Right (toks,e)

parseTerm :: Through [Token] ([Token], Expr)
parseTerm toks = do 
    (toks',expr) <- parseFactor toks 
    parseTerm' expr toks' 

  where 
    parseTerm' :: Expr -> Through [Token] ([Token],Expr)
    parseTerm' e (Operator Minus:toks) = do 
        (toks',expr) <- parseTerm toks 
        Right(toks',Sub e expr)
    parseTerm' e (Operator Plus:toks) = do 
        (toks',expr) <- parseTerm toks 
        Right(toks',Add e expr)
    parseTerm' e toks = Right (toks,e)

-- can definitely neaten this up later, but currently just want to get everything
-- working.
parseFactor :: Through [Token] ([Token], Expr)
parseFactor toks = do 
    (toks',expr) <- parseExponent toks 
    parseFactor' expr toks' 
  where 
    parseFactor' :: Expr -> Through [Token] ([Token],Expr)
    parseFactor' e (Operator Times:toks) = do 
        (toks',expr) <- parseFactor toks
        Right (toks',Mul e expr)
    parseFactor' e (Operator DivOp:toks) = do 
        (toks',expr) <- parseFactor toks
        Right (toks',Div e expr)
    parseFactor' e (Operator IntDivOp:toks) = do 
        (toks',expr) <- parseFactor toks
        Right (toks',IntDiv e expr)
    parseFactor' e (Operator ModOp:toks) = do 
        (toks',expr) <- parseFactor toks
        Right (toks',Mod e expr)
    parseFactor' e toks = Right (toks,e) 

parseExponent :: Through [Token] ([Token], Expr)
parseExponent toks = do 
    (toks',expr) <- parseAtom toks
    case toks' of 
        (Operator PowOp:toks'') -> do 
            (toks''',expr') <- parseFactor toks''
            Right (toks''', Pow expr expr')
        _ -> Right (toks',expr)


-- handles lowest level of expressions - strings, ints etc
parseAtom :: Through [Token] ([Token], Expr)
parseAtom (Ident x:toks) = Right(toks, Identifier x)
parseAtom (Val x:toks) = Right(toks, ValExp x)
parseAtom (Operator Minus:Val (Int x):toks) = Right(toks, ValExp (Int (negate x))) 
parseAtom (Operator Minus:Val (Float x):toks) = Right(toks, ValExp (Float (negate x))) 
parseAtom (Delimiter LParen: toks) = do 
    (toks', expr) <- parseExpr toks
    toks'' <- checkTok (Delimiter RParen) toks' 
    Right(toks'',expr)
parseAtom tks = Left (ParsingError $ ExprNotFound $ mHead tks)