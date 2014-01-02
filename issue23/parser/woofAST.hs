{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module WoofAST where

import Control.Applicative
import Control.Monad.State (MonadState(..), StateT(..))
import Data.List (nub)
import MTParser hiding (literal)

-- Woof BNF
-- Woof := Form(+)
-- Form := Symbol | Special | Application
-- Symbol := [a-zA-Z](+)
-- Special := '{' Define | Lambda '}'
-- Define := 'define' Symbol Form
-- Lambda := 'lambda' '{' Symbol(*) '}' Form(+)
-- Application := '(' Form(+) ')'
-- Whitespace := \s+
-- Comment := ';' (not '\n')(*)

data AST = ASymbol String
         | ALambda [String] [AST]
         | ADefine String AST
         | AApp AST [AST]
         deriving (Show, Eq)

-- a type class that toggles
class Switch f where
  switch :: f a -> f ()

instance Switch Maybe where
  switch (Just _) = Nothing
  switch Nothing = Just ()

instance (Functor m, Switch m) => Switch (StateT s m) where
  switch (StateT f) = StateT g
    where g s = fmap (const ((), s)) . switch $ f s

not1 :: (MonadState [t] m, Alternative m, Switch m) => m a -> m t
not1 p = switch p *> item

-- filter input by predicate
satisfy :: (MonadState [t] m, Alternative m) => (t -> Bool) -> m t
satisfy = flip check item

-- a parser that takes single legal token
literal :: (MonadState [t] m, Alternative m, Eq t) => t -> m t
literal c = satisfy (==c)

-- tokenizer
whitespace = some $ satisfy (flip elem " \t\n\r\f")
comment = pure (:) <*> literal ';' <*> (many $ (not1 $ literal '\n'))
junk = many $ whitespace <|> comment
tok p = p <* junk

opencurly = tok $ literal '{'
closecurly = tok $ literal '}'
openparen = tok $ literal '('
closeparen = tok $ literal ')'
symbol = tok $ some char
  where char = satisfy $ flip elem alphabets
        alphabets = ['a'..'z'] ++ ['A'..'Z']

-- syntatic parser
application = openparen
              *> pure AApp
              <*> form
              <*> many form
              <* closeparen
special = opencurly *> (define <|> lambda) <* closecurly
define = check (=="define") symbol
         *> pure ADefine
         <*> symbol
         <*> form
lambda = check (=="lambda") symbol
         *> opencurly
         *> pure ALambda
         <*> check distinct (many symbol)
         <*> (closecurly *> (some form))
form = fmap ASymbol symbol <|> special <|> application

endcheck = switch item
woof = junk *> (many form) <* endcheck

distinct xs = length xs == length (nub xs)
