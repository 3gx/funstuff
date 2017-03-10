
{-# LANGUAGE FlexibleInstances #-} 
{-# LANGUAGE OverlappingInstances #-} 
{-# LANGUAGE ScopedTypeVariables #-} 

import Data.Typeable

class ShowType x 
 where
  t :: x -> String

instance Typeable x => ShowType x
 where
  t x = show $ typeOf x

instance (ShowType x, ShowType y) => ShowType (x -> y)
 where
  t _ = "(" ++ t (undefined::x) ++ " -> " ++ t (undefined::y) ++ ")"

main = do
  print $ t (id :: Int -> Int)
  print $ t (fmap :: (Int -> Float) -> [Int] -> [Float])
