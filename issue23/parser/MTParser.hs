{-# LANGUAGE FlexibleContexts #-}

-- Parser implementation with monad transformer
module MTParser where

import Control.Applicative
import Control.Monad.State
import Control.Monad.Trans.Maybe
import MonadError

type Parser e t a = StateT [t] (MaybeT (Either e)) a



-- take the first token from input, if any
item :: (MonadState [t] m, Alternative m) => m t
item = get >>= \xs ->
       case xs of
         (t:ts) -> put ts *> pure t
         [] -> empty

-- filter input by predicate
check :: (Monad m, Alternative m) => (a -> Bool) -> m a -> m a
check p m = m >>= \x -> if (p x) then return x else empty

-- take the first input literal
literal :: (MonadState [t] m, Alternative m, Eq t) => t -> m t
literal x = check (==x) item

-- evaluate input with parser
runParser :: Parser e t a -> [t] -> Either e (Maybe (a, [t]))
runParser p xs = runMaybeT $ runStateT p xs
