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

--------------------

instance (Memo a, Memo b) => Memo (Either a b) where
  data Table (Either a b) w = TSum (Table a w) (Table b w)
  toTable f = TSum (toTable (f . Left)) (toTable (f . Right))
  fromTable (TSum t _) (Left  v) = fromTable t v
  fromTable (TSum _ t) (Right v) = fromTable t v

f1 :: Either Bool Bool -> Integer
f1 v = case v of 
        Left x -> if x then factorial 30000 else factorial 30
        Right x -> if x then fibonacci 30 else fibonacci 10

g1 = fromTable . toTable $ f1

--------------------

instance (Memo a, Memo b) => Memo (a,b) where 
  newtype Table (a,b) w = TProduct (Table a (Table b w))
  toTable f = TProduct (toTable (\x -> toTable (\y -> f (x,y))))
  fromTable (TProduct t) (x,y) = fromTable (fromTable t x) y


f2 :: (Bool,Bool) -> Integer
f2 (True,True) = factorial 30000
f2 (True,False) = factorial 30
f2 (False,True) = fibonacci 30
f2 (False,False) = fibonacci 10

g2 = fromTable . toTable $ f2


--------------------


