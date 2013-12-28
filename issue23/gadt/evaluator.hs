{-# LANGUAGE GADTs #-}

data Expr t where
  IntVal :: Int -> Expr Int
  BoolVal :: Bool -> Expr Bool
  AddInt :: Expr Int -> Expr Int -> Expr Int
  IsZero :: Expr Int -> Expr Bool
  If :: Expr Bool -> Expr t -> Expr t -> Expr t

evaluate :: Expr t -> t
evaluate (IntVal i) = i
evaluate (BoolVal b) = b
evaluate (AddInt (IntVal i1) (IntVal i2)) = i1 + i2
evaluate (IsZero expr) = evaluate expr == 0
evaluate (If pred t f) = if (evaluate pred) then (evaluate t) else (evaluate f)
