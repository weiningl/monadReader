{-# LANGUAGE GADTs #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

data Zero
data Succ n
data Nat = Zero | Succ Nat

type family Plus (a::Nat) (b::Nat) :: Nat
type instance Plus 'Zero n = n
type instance Plus ('Succ m) n = 'Succ (Plus m n)

type family (m::Nat) :< (n::Nat) :: Bool
type instance m :< 'Zero = 'False
type instance 'Zero :< ('Succ n) = 'True
type instance ('Succ m) :< ('Succ n) = m :< n


data NatSing (n::Nat) where
  ZeroSing :: NatSing 'Zero
  SuccSing :: NatSing n -> NatSing ('Succ n)

data List a (n :: Nat) where
  Nil :: List a 'Zero
  Cons :: a -> List a n -> List a ('Succ n)

headSafe :: List a ('Succ n) -> a
headSafe (Cons a _) = a

mapSafe :: (a -> b) -> List a n -> List b n
mapSafe _ Nil = Nil
mapSafe f (Cons a as) = Cons (f a) (mapSafe f as)

concatenate :: List a m -> List a n -> List a (Plus m n)
concatenate Nil xs = xs
concatenate (Cons x xs) ys = Cons x (concatenate xs ys)

repeatElem :: a -> NatSing n -> List a n
repeatElem _ ZeroSing = Nil
repeatElem a (SuccSing n) = Cons a (repeatElem a n)

nthElem :: (n :< m) ~ 'True => List a m -> NatSing n -> a
nthElem (Cons x xs) ZeroSing = x
nthElem (Cons x xs) (SuccSing n) = nthElem xs n
