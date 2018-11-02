module LambdaLex where

import Lex

data ExpTok
  = T_LPAREN | T_RPAREN | T_DOT | T_COLON
  | T_LAM | T_ID String
  | T_NUM Int | T_PLUS | T_TIMES | T_LESS
  | T_BOOL Bool | T_IF | T_THEN | T_ELSE
  | T_BOOL_TY | T_NUM_TY | T_ARROW
  deriving Eq

showExpTok :: ExpTok -> String
showExpTok T_LPAREN = "("
showExpTok T_RPAREN = ")"
showExpTok T_LAM = "\\"
showExpTok T_DOT = "."
showExpTok T_COLON = ":"
showExpTok (T_ID x) = x
showExpTok (T_NUM n) = show n
showExpTok T_PLUS = "+"
showExpTok T_TIMES = "*"
showExpTok T_LESS = "<"
showExpTok (T_BOOL b) = show b
showExpTok T_IF = "if"
showExpTok T_THEN = "then"
showExpTok T_ELSE = "else"
showExpTok T_BOOL_TY = "bool"
showExpTok T_NUM_TY = "num"
showExpTok T_ARROW = "->"

instance Show ExpTok where show = showExpTok

idStartRegex :: Regex
idStartRegex = range 'a' 'z' :|: range 'A' 'Z' :|: Sing '_'

idRegex :: Regex
idRegex = idStartRegex :.: star (idStartRegex :|: range '0' '9' :|: Sing '\'')

expLexGrammar :: LexGrammar ExpTok
expLexGrammar =
  [ (Sing '(',               \cs -> Just T_LPAREN),
    (Sing ')',               \cs -> Just T_RPAREN),
    (Sing '.',               \cs -> Just T_DOT),
    (Sing ':',               \cs -> Just T_COLON),
    (Sing '\\' :|: Sing 'λ', \cs -> Just T_LAM),
    (Sing '+',               \cs -> Just T_PLUS),
    (Sing '*',               \cs -> Just T_TIMES),
    (Sing '<',               \cs -> Just T_LESS),
    (string "true",          \cs -> Just (T_BOOL True)),
    (string "false",         \cs -> Just (T_BOOL False)),
    (string "if",            \cs -> Just T_IF),
    (string "then",          \cs -> Just T_THEN),
    (string "else",          \cs -> Just T_ELSE),
    (string "bool",          \cs -> Just T_BOOL_TY),
    (string "num",           \cs -> Just T_NUM_TY),
    (string "->",            \cs -> Just T_ARROW),
    (Plus (range '0' '9'),   \cs -> Just (T_NUM (read cs))),
    (idRegex,                \cs -> Just (T_ID cs)),
    (Plus whitespace,        \cs -> Nothing) ]

lexExp :: String -> [ExpTok]
lexExp cs =
  case tokenize expLexGrammar cs of
    (ts, "") -> ts
    (ts, cs') -> error ("leftover text after tokenizing: " ++ cs')
