module ParserCombinators where

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
  p >>= f = Parser $ \ts -> do
    (x, ts') <- runParser p ts
    runParser (f x) ts'

giveUp :: Parser t a
giveUp = Parser (const Nothing)

lookahead :: Parser t t
lookahead = Parser $ \ts ->
  case ts of
    [] -> Nothing
    (t:_) -> Just (t, ts)

infixr 2 <|>
(<|>) :: Parser t a -> Parser t a -> Parser t a
p <|> q = Parser $ \ts ->
  case runParser p ts of
    Just xts -> Just xts
    Nothing -> runParser q ts

eat :: Eq t => t -> Parser t ()
eat x = Parser $ \ts ->
  case ts of
    [] -> Nothing
    (t:ts)
      | x == t -> Just ((), ts)
      | otherwise -> Nothing

advance :: Eq t => Parser t t
advance = do
  x <- lookahead
  eat x
  return x
