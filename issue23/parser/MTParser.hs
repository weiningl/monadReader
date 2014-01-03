{-# LANGUAGE FlexibleContexts #-}

-- Parser implementation with monad transformer
module MTParser where

import Control.Applicative
import Control.Monad.State
import Control.Monad.Trans.Maybe
import MonadError

type Pos = (Int, Int)
type Err = (String, Pos)
type Parser t a = StateT [t] (StateT Pos (MaybeT (Either Err))) a

-- state retrieval/update
getState :: Parser Char Pos
getState = lift get

putState :: Pos -> Parser Char ()
putState = lift . put

updateState :: (Pos -> Pos) -> Parser Char ()
updateState f = getState >>= (putState . f)

-- take the first token from input, if any
basic_item :: (MonadState [t] m, Alternative m) => m t
basic_item = get >>= \xs -> case xs of
    (t:ts) -> put ts *> pure t
    [] -> empty

item :: Parser Char Char
item = basic_item >>= \x -> updateState (f x) >> return x
  where f '\n' (r, c) = (r + 1, 1)
        f _ (r, c) = (r, c + 1)

-- filter input by predicate
check :: (Monad m, Alternative m) => (a -> Bool) -> m a -> m a
check p m = m >>= \x -> if (p x) then return x else empty

-- take the first input literal
literal :: Char -> Parser Char Char
literal x = check (==x) item

-- evaluate input with parser
runParser :: Parser t a -> [t] -> Either Err (Maybe ((a, [t]), Pos))
runParser p xs = runMaybeT $ runStateT (runStateT p xs) (1, 1)
