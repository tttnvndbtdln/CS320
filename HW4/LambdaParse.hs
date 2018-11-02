module LambdaParse where

import Control.Applicative
import Lambda
import LambdaLex
import ParserCombinators

readExp :: String -> Exp
readExp cs =
  case runParser parseExp (lexExp cs) of
    Just (p, []) -> p
    Just (p, ts) -> error ("leftover tokens after parsing: " ++ show ts)
    Nothing -> error ("couldn't parse: " ++ cs)

parseTy = parseFunctionTy <|> parseAtomicTy

parseAtomicTy = parsePrimitiveTy <|> parseParenTy

parsePrimitiveTy = do
  t <- advance
  case t of
    T_BOOL_TY -> return BOOL
    T_NUM_TY -> return NUM
    _ -> giveUp

parseFunctionTy = do
  t1 <- parseAtomicTy
  eat T_ARROW
  t2 <- parseFunctionTy <|> parseAtomicTy
  return (t1 :-> t2)

parseParenTy = do
  eat T_LPAREN
  t <- parseTy
  eat T_RPAREN
  return t

parseExp = choice
  [ parseIf,
    parseLam,
    parseInequality,
    parseApp ]

parseId = do
  x <- advance
  case x of
    T_ID x' -> return x'
    _ -> giveUp

parseApp = do
  e <- parseAtom
  es <- some parseAtom
  return (foldl1 App (e:es))

parseUntypedArg = do
  x <- parseId
  return (x, Nothing)

parseTypedArg = do
  eat T_LPAREN
  x <- parseId
  eat T_COLON
  t <- parseTy
  eat T_RPAREN
  return (x, Just t)

parseLam = do
  eat T_LAM
  xts <- some (parseUntypedArg <|> parseTypedArg)
  eat T_DOT
  e <- parseExp
  return (foldr (uncurry Lam) e xts)

parseParens = eat T_LPAREN *> parseExp <* eat T_RPAREN

parseIf = do
  eat T_IF
  e1 <- parseExp
  eat T_THEN
  e2 <- parseExp
  eat T_ELSE
  e3 <- parseExp
  return (If e1 e2 e3)

parseOp o t p q = do
  e1 <- p
  eat t
  e2 <- q
  return (e1 `o` e2)

parseLess = parseOp Less T_LESS parseSum parseInequality

parseInequality = parseLess <|> parseSum

parsePlus = parseOp Plus T_PLUS parseProduct parseSum

parseSum = parsePlus <|> parseProduct

parseTimes = parseOp Times T_TIMES parseAtom parseProduct

parseProduct = parseTimes <|> parseApp <|> parseAtom

parseAtom = choice
  [ parseParens,
    Var <$> parseId,
    parseLit ]

parseLit = do
  e <- advance
  case e of
    T_NUM n -> return (Num n)
    T_BOOL b -> return (Bool b)
    _ -> giveUp
