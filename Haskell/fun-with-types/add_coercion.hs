{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
 {-# LANGUAGE FlexibleContexts #-}

class Add a b where
  type SumTy a b
  add :: a -> b -> SumTy a b

instance Add Integer Double where
  type SumTy Integer Double = Double
  add x y = fromIntegral x  + y

instance Add Double Integer where
  type SumTy Double Integer = Double
  add x y = x + fromIntegral y

-- requires FlexibleInstances
instance (Num a) => Add a a where
  type SumTy a a = a
  add x y = x + y

-- requires FlexibleContexts
instance (Add Integer a) => Add Integer [a] where
  type SumTy Integer [a] = [SumTy Integer a]
  add x y = map (add x) y

main = do
  print $ add (3 :: Integer) (3 :: Integer)
  print $ add (3.2 :: Double) (3 :: Integer)
  print $ add (3 :: Integer) (5.2 :: Double)
  print $ add (3 :: Integer) ([5.2,3.2,4.4] :: [Double])
