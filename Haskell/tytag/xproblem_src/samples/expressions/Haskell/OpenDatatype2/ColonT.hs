-- Configurable ":t"

{-# LANGUAGE FlexibleInstances #-} 
{-# LANGUAGE UndecidableInstances #-} 
{-# LANGUAGE OverlappingInstances #-} 
{-# LANGUAGE ScopedTypeVariables #-} 

module ColonT where

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
