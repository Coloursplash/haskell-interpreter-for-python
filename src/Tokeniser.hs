module Tokeniser (tokenise) where

import Data.Char (isAlphaNum, isDigit, isLetter, isSpace)
import Data.List (isPrefixOf, maximumBy)
import Data.Ord (comparing)
import Types

opDelimTable :: [(String, Token)]
opDelimTable =
  [ ("+", Operator Plus),
    ("-", Operator Minus),
    ("*", Operator Times),
    ("**", Operator Pow),
    ("/", Operator DivOp),
    ("//", Operator IntDiv),
    ("%", Operator Mod),
    ("@", Operator At),
    ("<<", Operator ShiftL),
    (">>", Operator ShiftR),
    ("&&", Operator AndOp),
    ("|", Operator Pipe),
    ("^", Operator Hat),
    ("~", Operator Tilde),
    (":=", Operator AssignOp),
    ("<", Operator LessThan),
    (">", Operator GreaterThan),
    ("<=", Operator LTEq),
    (">=", Operator GTEq),
    ("==", Operator Eq),
    ("!=", Operator NotEq),
    ("(", Delimiter LParen),
    (")", Delimiter RParen),
    ("[", Delimiter LSquare),
    ("]", Delimiter RSquare),
    ("{", Delimiter LBrace),
    ("}", Delimiter RBrace),
    (",", Delimiter Comma),
    (":", Delimiter Colon),
    ("!", Delimiter Exclamation),
    (".", Delimiter Period),
    (";", Delimiter Semi),
    ("=", Delimiter EqDelim),
    ("->", Delimiter ArrowRight),
    ("+=", Delimiter PlusEq),
    ("-=", Delimiter MinusEq),
    ("*=", Delimiter TimesEq),
    ("/=", Delimiter DivEq),
    ("//=", Delimiter IntDivEq),
    ("%=", Delimiter ModEq),
    ("@=", Delimiter AtEq),
    ("&=", Delimiter AndEq),
    ("|=", Delimiter PipeEq),
    ("^=", Delimiter HatEq),
    (">>=", Delimiter ShiftREq),
    ("<<=", Delimiter ShiftLEq),
    ("**=", Delimiter PowEq)
  ]

keywordTable :: [(String, Token)]
keywordTable =
  [ ("and", Keyword And),
    ("as", Keyword As),
    ("assert", Keyword Assert),
    ("async", Keyword Async),
    ("await", Keyword Await),
    ("break", Keyword Break),
    ("class", Keyword Class),
    ("continue", Keyword Continue),
    ("def", Keyword Def),
    ("del", Keyword Del),
    ("elif", Keyword Elif),
    ("else", Keyword Else),
    ("except", Keyword Except),
    ("finally", Keyword Finally),
    ("for", Keyword For),
    ("from", Keyword From),
    ("global", Keyword Global),
    ("if", Keyword If),
    ("import", Keyword Import),
    ("in", Keyword In),
    ("is", Keyword Is),
    ("lambda", Keyword Lambda),
    ("nonlocal", Keyword Nonlocal),
    ("not", Keyword Not),
    ("or", Keyword Or),
    ("pass", Keyword Pass),
    ("raise", Keyword Raise),
    ("return", Keyword Return),
    ("try", Keyword Try),
    ("while", Keyword WhileTok),
    ("with", Keyword With),
    ("yield", Keyword Yield)
  ]

-- | Tokenises the input string into a list of tokens
tokenise :: Through String [Token]
tokenise inp = tokenise' inp (0, 0) []
  where
    -- tokenise' :: text -> (prevIndent, curIndent) -> Through [Token] [Token]
    tokenise' :: String -> (Int, Int) -> Through [Token] [Token]
    tokenise' [] _ toks = Right (reverse toks)
    tokenise' inp@(c : cs) (prevIndent, currIndent) toks
      | isSpace c =
          let (spaces, rest) = span isSpace inp
              newIndent = length spaces
           in if c == '\n'
                then handleIndent newIndent prevIndent rest toks
                else tokenise' rest (prevIndent, newIndent) toks
      | isDigit c || (c == '-' && not (null cs) && isDigit (head cs)) =
          let (t, cs') = span (\x -> isDigit x || x == '-') inp
           in tokenise' cs' (prevIndent, currIndent) (Val (Int (read t)) : toks)
      | isLetter c =
          let (t, cs') = span isAlphaNum inp
           in maybe
                (tokenise' cs' (prevIndent, currIndent) (Ident t : toks))
                (\op -> tokenise' cs' (prevIndent, currIndent) (op : toks))
                (lookup t keywordTable)
      | otherwise =
          let (t, cs') = extractOperator inp
           in maybe
                (Left (TokenisationError (UnrecognizedOperator t)))
                (\op -> tokenise' cs' (prevIndent, currIndent) (op : toks))
                (lookup t opDelimTable)

    handleIndent :: Int -> Int -> String -> Through [Token] [Token]
    handleIndent newIndent prevIndent rest toks
      | newIndent > prevIndent = tokenise' rest (prevIndent, newIndent) (BlockStart : toks)
      | newIndent < prevIndent = tokenise' rest (prevIndent, newIndent) (BlockEnd : toks)
      | otherwise = tokenise' rest (prevIndent, newIndent) toks

    extractOperator :: String -> (String, String)
    extractOperator s =
      let validOps = filter (`isPrefixOf` s) (map fst opDelimTable)
       in case validOps of
            [] -> ([head s], tail s) -- No match, take single char
            ops ->
              let best = maximumBy (comparing length) ops
               in (best, drop (length best) s)