{-# LANGUAGE GADTs #-}

data Type t where
  TInt :: Type Int
  TChar :: Type Char
  TList :: Type t -> Type [t]
  TDyn :: Type Dynamic

tString :: Type String
tString = TList TChar

data Bit = T | F deriving (Show)

encode :: Type t -> t -> [Bit]
encode TInt i = encodeInt i
encode TChar c = encodeChar c
encode (TList _) [] = F : []
encode (TList t) (x : xs) = T : (encode t x) ++ encode (TList t) xs
encode TDyn (Dyn t v) = encode t v

encodeInt :: Int -> [Bit]
encodeInt i = map toBit $ encodeInt' i []
  where encodeInt' i acc =
          if (i == 0) then acc
          else let (q, r) = i `divMod` 2
               in encodeInt' q (r : acc)
        toBit i = if (i == 1) then T else F

encodeChar :: Char -> [Bit]
encodeChar = encodeInt . fromEnum

data Dynamic where
  Dyn :: Type t -> t -> Dynamic

encode' :: Dynamic -> [Bit]
encode' (Dyn t v) = encode t v
