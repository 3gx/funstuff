-- 14.1 Monoids

{-
  class Monoid a where
    mempty :: a
    mappend :: a -> a -> a

    mconcat :: [a] -> a
    mconcat = foldr mappend mempry
-}


instance Monoid Int where
  mempty = 0
  mappend = (+)

-- > (3::Int) `mappend` 5
-- 8
-- > (3::Int) <> 5
-- 8


instance Monoid Integer where
  mempty = 1
  mappend = (*)

-- > 3 `mappend` 5
-- 15
-- > 3 <> 5
-- 15



-- fold behaves in the same way mconcat 

fold :: Monoid a => [a] -> a
fold [] = mempty
fold (x:xs) = x `mappend` fold xs

-- > fold [3,4,5,6,7]
-- 2520
-- > fold [3::Int,4,5,6,7]
-- 25



