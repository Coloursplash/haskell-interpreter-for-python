{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Parser (parse, parseExpr, parseStmt, parseIterable) where

import GHC.IO (unsafePerformIO)
import Tokeniser (tokenise)
import Types

mHead :: [a] -> Maybe a
mHead (x : _) = Just x
mHead _ = Nothing

checkTok :: Token -> Through [Token] [Token]
checkTok tk (tk' : toks)
  | tk == tk' = Right toks
checkTok tk toks = Left (ParsingError (Unexpected (mHead toks) tk))

-- | Parse the tokenised input into an AST
parse :: Through [Token] Block
parse [] = Right []
parse toks = do
  (toks', b) <- parseBlock (toks ++ [BlockEnd])
  b' <- parse toks'
  Right (b ++ b')

parseBlock :: Through [Token] ([Token], Block)
parseBlock toks = parseBlock' [] (BlockStart : toks)
  where
    parseBlock' :: Block -> Through [Token] ([Token], Block)
    parseBlock' b (BlockStart : toks) = do
      (toks', stmt) <- parseStmt toks
      parseBlock' (b ++ [stmt]) toks'
    parseBlock' b (BlockEnd : toks) = Right (toks, b)
    parseBlock' b [] = Left (ParsingError (Unexpected Nothing BlockEnd))
    parseBlock' b toks = do
      (toks', stmt) <- parseStmt toks
      parseBlock' (b ++ [stmt]) toks'

-- Still need to add in parsing for loops
parseStmt :: Through [Token] ([Token], Stmt)
parseStmt (Ident x : Delimiter EqDelim : toks) = do
  (toks', expr) <- parseExpr toks
  Right (toks', Asgn x expr)
parseStmt (Ident "print" : Delimiter LParen : toks) = do
  parseIterable (Delimiter LParen) (Delimiter RParen) Print parseExpr [] (Delimiter LParen : toks)
parseStmt tks@(Ident x : Delimiter LParen : toks) = do
  (toks', expr) <- parseExpr tks
  Right (toks', ExprStmt expr)
parseStmt (Ident x : tk : toks) = case lookup tk parseStmtLookup of
  Just eConst -> do
    (toks', expr) <- parseExpr toks
    Right (toks', Asgn x (eConst (Identifier x) expr))
  Nothing -> do
    (toks', expr) <- parseExpr (Ident x : tk : toks)
    Right (toks', ExprStmt expr)
parseStmt (Keyword WhileTok : toks) = do
  (toks', expr) <- parseExpr toks
  (toks'', b) <-
    checkTok (Delimiter Colon) toks'
      >>= checkTok BlockStart
      >>= parseBlock
  Right (toks'', While expr b)
-- currently assuming there will always be an else... clause
-- when implemented 'elif's will be another if ... else statement
-- that will be added to the else block
parseStmt (Keyword If : toks) = do
  (toks1, expr) <- parseExpr toks
  (toks2, b1) <-
    checkTok (Delimiter Colon) toks1
      >>= checkTok BlockStart
      >>= parseBlock
  case toks2 of 
    (Keyword Else:toks3) -> do
      (toks4, b2) <- checkTok (Delimiter Colon) toks3 >>= checkTok BlockStart >>= parseBlock
      Right (toks4, Cond expr b1 b2)
    (Keyword Elif:toks3) -> do 
      (toks4,expr') <- parseExpr toks3
      (toks5, stmt) <- parseStmt (Keyword If : toks4)
      Right (toks5, Cond expr b1 [stmt])
    toks3 -> do 
      Right (toks3, Cond expr b1 [])

parseStmt (Keyword Return : toks) = do
  (toks', expr) <- parseExpr toks
  Right (toks', Ret expr)
parseStmt (Keyword Def : Ident funcName : toks) = do
  -- A way to get a list of expressions without changing parseIterable/making a
  -- new helper function
  (toks', expr) <- parseIterable (Delimiter LParen) (Delimiter RParen) (ValExp . List) parseAtom [] toks
  toks'' <- checkTok (Delimiter Colon) toks' >>= checkTok BlockStart
  case expr of
    ValExp (List es) -> do
      (toks''', b) <- parseBlock toks''
      strs <- getStrings es []
      Right (toks''', FuncDef funcName strs b)
        where
          getStrings :: [Expr] -> Through [String] [String]
          getStrings [] strs = Right $ reverse strs 
          getStrings (e:es) strs = 
            case e of 
              Identifier x -> getStrings es (x:strs)
              x -> Left $ ParsingError $ InvalidTypeError $ "Expected type Identifier, but got " ++ showType x ++ "." 
    _ -> Left $ ParsingError UnknownError
parseStmt (Keyword Def : toks) = Left $ ParsingError $ SyntaxError "Def keyword must be followed by the name of a function" 
parseStmt (Keyword For : Ident x : Keyword In : toks) = do 
  (toks',expr) <- parseExpr toks 
  (toks'',b) <- checkTok (Delimiter Colon) toks' >>= checkTok BlockStart >>= parseBlock
  Right (toks'', ForLoop x expr b)
parseStmt (Keyword For : toks) = Left $ ParsingError $ SyntaxError "For keyword must be followed by some form of 'i in ..."
parseStmt toks = do
  (toks', expr) <- parseExpr toks
  Right (toks', ExprStmt expr)

parseStmtLookup :: [(Token, Expr -> Expr -> Expr)]
parseStmtLookup =
  [ (Delimiter PlusEq, Add),
    (Delimiter MinusEq, Sub),
    (Delimiter TimesEq, Mul),
    (Delimiter DivEq, Div),
    (Delimiter IntDivEq, IntDiv),
    (Delimiter ModEq, Mod),
    (Delimiter PowEq, Pow),
    (Delimiter AtEq, At),
    (Delimiter AndEq, AndExp),
    (Delimiter PipeEq, Pipe),
    (Delimiter HatEq, Hat),
    (Delimiter ShiftLEq, ShiftL),
    (Delimiter ShiftREq, ShiftR)
  ]

parseExpr :: Through [Token] ([Token], Expr)
parseExpr (Ident "input" : Delimiter LParen : toks) = do
  parseIterable (Delimiter LParen) (Delimiter RParen) Input parseExpr [] (Delimiter LParen : toks)
parseExpr (Keyword Not : toks) = do
  (toks', comp) <- parseComparison toks
  Right (toks', NotExp comp)
parseExpr toks = do
  (toks', expr) <- parseComparison toks
  case toks' of
    [] -> Right (toks', expr)
    (tk : toks'') -> case lookup tk parseExprLookup of
      Just eConst -> do
        (toks''', expr') <- parseExpr toks''
        Right (toks''', eConst expr expr')
      Nothing -> Right (toks', expr)

parseExprLookup :: [(Token, Expr -> Expr -> Expr)]
parseExprLookup =
  [ (Keyword And, AndExp),
    (Operator AndOp, AndExp),
    (Keyword Or, Pipe),
    (Operator PipeOp, Pipe)
  ]

parseComparison :: Through [Token] ([Token], Expr)
parseComparison toks = do
  (toks', term) <- parseTerm toks
  case toks' of
    [] -> Right (toks', term)
    (tk : toks'') -> case lookup tk parseComparisonLookup of
      Just eConst -> do
        (toks''', expr') <- parseTerm toks''
        Right (toks''', eConst term expr')
      Nothing -> Right (toks', term)

parseComparisonLookup :: [(Token, Expr -> Expr -> Expr)]
parseComparisonLookup =
  [ (Operator LessThanOp, LessThan),
    (Operator GreaterThanOp, GreaterThan),
    (Operator LTEqOp, LTEq),
    (Operator GTEqOp, GTEq),
    (Operator EqOp, Eq),
    (Operator NotEqOp, NotEq),
    (Keyword Is, Eq),
    (Keyword Not, NotEq)
  ]

parseTerm :: Through [Token] ([Token], Expr)
parseTerm toks = do
  (toks', expr) <- parseFactor toks
  parseTerm' expr toks'
  where
    parseTerm' :: Expr -> Through [Token] ([Token], Expr)
    parseTerm' e (Operator Minus : toks) = do
      (toks', expr) <- parseTerm toks
      Right (toks', Sub e expr)
    parseTerm' e (Operator Plus : toks) = do
      (toks', expr) <- parseTerm toks
      Right (toks', Add e expr)
    parseTerm' e toks = Right (toks, e)

-- can definitely neaten this up later, but currently just want to get everything
-- working.
parseFactor :: Through [Token] ([Token], Expr)
parseFactor toks = do
  (toks', expr) <- parseExponent toks
  parseFactor' expr toks'
  where
    parseFactor' :: Expr -> Through [Token] ([Token], Expr)
    parseFactor' e (Operator Times : toks) = do
      (toks', expr) <- parseFactor toks
      Right (toks', Mul e expr)
    parseFactor' e (Operator DivOp : toks) = do
      (toks', expr) <- parseFactor toks
      Right (toks', Div e expr)
    parseFactor' e (Operator IntDivOp : toks) = do
      (toks', expr) <- parseFactor toks
      Right (toks', IntDiv e expr)
    parseFactor' e (Operator ModOp : toks) = do
      (toks', expr) <- parseFactor toks
      Right (toks', Mod e expr)
    parseFactor' e toks = Right (toks, e)

parseExponent :: Through [Token] ([Token], Expr)
parseExponent toks = do
  (toks', expr) <- parseAtom toks
  case toks' of
    (Operator PowOp : toks'') -> do
      (toks''', expr') <- parseFactor toks''
      Right (toks''', Pow expr expr')
    _ -> Right (toks', expr)

-- handles lowest level of expressions - strings, ints etc
parseAtom :: Through [Token] ([Token], Expr)
parseAtom (Ident x : Delimiter LParen : toks) = do
  parseIterable (Delimiter LParen) (Delimiter RParen) (FunctionCall x) parseExpr [] (Delimiter LParen : toks)
parseAtom (Ident x : toks) = Right (toks, Identifier x)
parseAtom (Val x : toks) = Right (toks, ValExp x)
parseAtom (Operator Minus : Val (Int x) : toks) = Right (toks, ValExp (Int (negate x)))
parseAtom (Operator Minus : Val (Float x) : toks) = Right (toks, ValExp (Float (negate x)))
parseAtom (Delimiter LParen : toks) = do
  (toks', expr) <- parseExpr toks
  toks'' <- checkTok (Delimiter RParen) toks'
  Right (toks'', expr)
parseAtom tks@(Delimiter LSquare : toks) = do
  parseIterable (Delimiter LSquare) (Delimiter RSquare) (ValExp . List) parseAtom [] tks
parseAtom tks@(Delimiter LBrace : toks) = do
  parseIterable (Delimiter LBrace) (Delimiter RBrace) (ValExp . Dict) parsePair [] tks
parseAtom tks = Left (ParsingError $ ExprNotFound $ mHead tks)

parsePair :: Through [Token] ([Token], (Expr, Expr))
parsePair toks = do
  (toks', key) <- parseAtom toks
  (toks'', value) <-
    checkTok (Delimiter Colon) toks'
      >>= parseAtom
  Right (toks'', (key, value))

parseIterable :: Token -> Token -> ([a] -> b) -> Through [Token] ([Token], a) -> [a] -> Through [Token] ([Token], b)
parseIterable startTok endTok eConst func exprs (tok : toks)
  | tok == startTok = do
      (toks', expr) <- func toks
      parseIterable startTok endTok eConst func (expr : exprs) toks'
  | tok == endTok = Right (toks, eConst $ reverse exprs)
parseIterable startTok endTok eConst func exprs (Delimiter Comma : toks) = do
  (toks', expr) <- func toks
  parseIterable startTok endTok eConst func (expr : exprs) toks'
parseIterable startTok endTok eConst func exprs toks = Left $ ParsingError (Unexpected (mHead toks) endTok)