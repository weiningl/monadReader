{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

-- Red/Black Tree invariants
-- 1) Root is black
-- 2) Leaf is black
-- 3) Red node has only black children
-- 4) For each node, the path to each leaf contains the same number of black nodes

data Nat = Zero | Succ Nat

data Color = R | B

data ColorSingleton (c::Color) where
  SR :: ColorSingleton R
  SB :: ColorSingleton B

class ValidColors (parent::Color) (child1::Color) (child2::Color)
instance ValidColors R B B
instance ValidColors B c1 c2

type family IncBlackHeight (c::Color) (bh::Nat) :: Nat
type instance IncBlackHeight R bh = bh
type instance IncBlackHeight B bh = Succ bh

data Node a (bh::Nat) where
  E :: Node a 'Zero
  N :: ColorSingleton c
       -> Node a bh
       -> a
       -> Node a bh
       -> Node a (IncBlackHeight c bh)

data Tree a where
  Root :: Node a bh -> Tree a

member :: (Ord a) => a -> Tree a -> Bool
member x (Root t) = member' x t

member' :: (Ord a) => a -> Node a bh -> Bool
member' _ E = False
member' x (N _ l a r)
  | x < a = member' x l
  | x > a = member' x r
  | otherwise = True

insert :: (Ord a) => Tree a -> a -> Tree a
insert (Root t) v = blacken $ insertInternal t v
  where insertInternal :: (Ord a) => Node a n -> a -> Node a n
        insertInternal E x = N SR E x E
        insertInternal n@(N c l a r) x
          | x < a = leftBalance $ N c (insertInternal l x) a r
          | x > a = rightBalance $ N c l a (insertInternal r x)
          | otherwise = n
        blacken (N _ l a r) = Root $ N SB l a r

leftBalance :: Node a bh -> Node a bh
leftBalance (N SB (N SR (N SR a x b) y c) z d) =
  N SR (N SB a x b) y (N SB c z d)
leftBalance (N SB (N SR a x (N SR b y c)) z d) =
  N SR (N SB a x b) y (N SB c z d)
leftBalance n = n

rightBalance :: Node a bh -> Node a bh
rightBalance (N SB a x (N SR b y (N SR c z d))) =
  N SR (N SB a x b) y (N SB c z d)
rightBalance (N SB a x (N SR (N SR b y c) z d)) =
  N SR (N SB a x b) y (N SB c z d)
rightBalance n = n
