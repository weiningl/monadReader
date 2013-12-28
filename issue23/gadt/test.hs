{-# LANGUAGE GADTs #-}

data Test a where
  TI :: Int -> Test Int
  TS :: String -> a -> Test a

f :: Test a -> a
f (TI a) = a + 10
