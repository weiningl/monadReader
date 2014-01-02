-- A parser that processes a list of tokens; may fail
module Parser where

import Control.Applicative


newtype Parser t a = Parser { runParser :: [t] -> Maybe ([t], a) }

instance Functor (Parser t) where
  fmap f (Parser p) = Parser $ fmap (fmap f) . p

instance Applicative (Parser t) where
  pure a = Parser $ \ts -> Just (ts, a)
  (Parser p1) <*> (Parser p2) = Parser p3
    where p3 xs = case (p1 xs) of
            Nothing -> Nothing
            Just (ys, f) -> fmap (fmap f) (p2 ys)

instance Alternative (Parser t) where
  empty = Parser (const empty)
  Parser l <|> Parser r = Parser $ \ts -> l ts <|> r ts

instance Monad (Parser t) where
  return a = Parser $ \ts -> Just (ts, a)
  (Parser p) >>= f = Parser $ \ts -> (p ts) >>= \(ts', a) ->
    runParser (f a) ts'

-- a parser that takes a token, if successful
item :: Parser t t
item = Parser f
  where f [] = Nothing
        f (x:xs) = Just (xs, x)

-- filter input by predicate
check :: (Monad f, Alternative f) => (a -> Bool) -> f a -> f a
check p m = m >>= \x -> if (p x) then return x else empty

-- check single token
literal :: (Eq t) => t -> Parser t t
literal x = check (==x) item

-- testing
