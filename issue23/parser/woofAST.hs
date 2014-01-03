{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeFamilies #-}

module WoofAST where

import Control.Applicative
import Control.Monad.Trans.Maybe (MaybeT(..))
import Control.Monad.State (MonadState(..), StateT(..))
import Data.List (nub)
import MTParser hiding (literal)
import MonadError

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

-- Error messages
eAppOper = "application: missing operator"
eAppClose = "application: missing close parenthesis"
eDefSym = "define: missing symbol"
eDefForm = "define: missing form"
eLamParam = "lambda: missing parameter list"
eLamDupe = "lambda: duplicated parameter names"
eLamPClose = "lambda: missing parameter close curly"
eLamBody = "lambda: missing body"
eSpecClose = "special form: missing close curly"
eSpecial = "special form: unable to parse"
eWoof = "woof: unparsed input"

-- conditionally throw an error
commit :: (MonadError m, Alternative m) => Error m -> m a -> m a
commit e m = m <|> throwError e

cut :: String -> Parser Char a -> Parser Char a
cut msg p = do
  pos <- getState
  commit (msg, pos) p

-- a type class that toggles
class Switch f where
  switch :: f a -> f ()

instance Switch Maybe where
  switch (Just _) = Nothing
  switch Nothing = Just ()

instance (Functor m) => Switch (MaybeT m) where
  switch (MaybeT m) = MaybeT (fmap switch m)

instance (Functor m, Switch m) => Switch (StateT s m) where
  switch (StateT f) = StateT g
    where g s = fmap (const ((), s)) . switch $ f s

-- not1 :: (MonadState [t] m, Alternative m, Switch m) => m a -> m t
not1 :: Parser Char Char -> Parser Char Char
not1 p = switch p *> item

-- filter input by predicate
-- satisfy :: (MonadState [t] m, Alternative m) => (t -> Bool) -> m t
satisfy :: (Char -> Bool) -> Parser Char Char
satisfy = flip check item

-- a parser that takes single legal token
-- literal :: (MonadState [t] m, Alternative m, Eq t) => t -> m t
literal :: Char -> Parser Char Char
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

application = do
  openparen
  op <- cut eAppOper form
  args <- many form
  cut eAppClose closeparen
  return $ AApp op args

define = check (=="define") symbol
         *> pure ADefine
         <*> cut eDefSym symbol
         <*> cut eDefForm form

lambda = do
  check (=="lambda") symbol
  cut eLamParam opencurly
  params <- many symbol
  -- cut eLamDupe (check distinct params)
  if distinct params then return () else cut eLamDupe empty
  cut eLamPClose closecurly
  bodies <- cut eLamBody (some form)
  return $ ALambda params bodies

special = opencurly
          *> cut eSpecial (define <|> lambda)
          <* cut eSpecClose closecurly

form = fmap ASymbol symbol <|> special <|> application

endcheck = switch item
woof = junk *> (many form) <* cut eWoof endcheck

distinct xs = length xs == length (nub xs)
