module ParserCombinators where

import Control.Applicative

newtype Parser t a = Parser { runParser :: [t] -> Maybe (a, [t]) }

mapFst :: (a -> b) -> (a, c) -> (b, c)
mapFst f (x, y) = (f x, y)

instance Functor (Parser t) where
  fmap f p = Parser (fmap (mapFst f) . runParser p)

instance Applicative (Parser t) where
  pure x = Parser $ \ts -> Just (x, ts)
  p <*> q = Parser $ \ts -> do
    (f, ts') <- runParser p ts
    (x, ts'') <- runParser q ts'
    return (f x, ts'')

instance Monad (Parser t) where
  return = pure
  p >>= f = Parser $ \ts -> do
    (x, ts') <- runParser p ts
    runParser (f x) ts'

giveUp :: Parser t a
giveUp = Parser (const Nothing)

instance Alternative (Parser t) where
  empty = giveUp
  p <|> q = Parser $ \ts ->
    case runParser p ts of
      Just xts -> Just xts
      Nothing -> runParser q ts

lookahead :: Parser t t
lookahead = Parser $ \ts ->
  case ts of
    [] -> Nothing
    (t:_) -> Just (t, ts)

eat :: Eq t => t -> Parser t t
eat x = Parser $ \ts ->
  case ts of
    [] -> Nothing
    (t:ts)
      | x == t -> Just (t, ts)
      | otherwise -> Nothing

string :: Eq t => [t] -> Parser t ()
string [] = return ()
string (t:ts) = eat t >> string ts

advance :: Eq t => Parser t t
advance = do
  x <- lookahead
  eat x
  return x

choice :: [Parser t a] -> Parser t a
choice = foldr (<|>) empty

sepBy :: Parser t a -> Parser t b -> Parser t [a]
sepBy p q = do
  x <- optional p
  case x of
    Just x' -> do
      xs <- many (q *> p)
      return (x':xs)
    Nothing -> return []
