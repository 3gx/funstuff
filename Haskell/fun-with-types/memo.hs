{-# LANGUAGE TypeFamilies #-}

class Memo a where
  data Table a :: * -> *
  toTable :: (a -> w) -> Table a w
  fromTable :: Table a w -> (a -> w)

instance Memo Bool where
  data Table Bool w = TBool w w
  toTable f = TBool (f True) (f False)
  fromTable (TBool x y) b = if b then x else y

g :: Bool -> Integer
g = fromTable (toTable f)

factorial n | n < 1 = 1 | otherwise = n * factorial (n-1)
fibonacci n | n < 2 = 1 | otherwise = fibonacci(n-1) + fibonacci(n-2)
f True = factorial 30000
f False = fibonacci 30
