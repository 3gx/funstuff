-- 14.1 Monoids

import Data.Foldable

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

fold' :: Monoid a => [a] -> a
fold' [] = mempty
fold' (x:xs) = x `mappend` fold' xs

-- > fold [3,4,5,6,7]
-- 2520
-- > fold [3::Int,4,5,6,7]
-- 25

data Tree a = Leaf a | Node (Tree a) (Tree a) deriving Show

instance Foldable Tree where
  fold (Leaf x) = x
  fold (Node l r) = fold l `mappend` fold r

  foldMap f (Leaf x) = f x
  foldMap f (Node l r) = foldMap f l `mappend` foldMap f r

  foldr f v (Leaf x) = f x v
  foldr f v (Node l r) = foldr f (foldr f v r) l
  
  foldl f v (Leaf x) = f v x
  foldl f v (Node l r) = foldl f(foldl f v l) r

tree :: Tree Int
tree = Node (Node (Leaf 3) (Leaf 4)) (Leaf 5)

tree' :: Tree Integer
tree' = Node (Node (Leaf 3) (Leaf 4)) (Leaf 5)

-- > fold tree
-- 12
-- > fold tree'
-- 60

-- Generic functions

average :: Foldable t => t Int -> Int
average ns = sum ns `div` length ns

-- > average tree
-- 4

{-
   concat :: Foldatble t => t [a] -> [a]
   concat = fold
-}

-- > concat (Node (Leaf [1,2]) (Leaf [3,4]))
-- [1,2,3,4]

-- 14.3 Traversables

traverse' :: (a -> Maybe b) -> [a] -> Maybe [b]
traverse' g [] = pure []
traverse' g (x:xs) = pure (:) <*> g x <*> traverse' g xs

dec :: (Num a,Ord a) => a -> Maybe a
dec n = if n > 0 then Just (n-1) else Nothing

-- traverse' dec [1,2,3]
-- Just [0,1,2]
-- > traverse' dec [0,1,2]
-- Nothing

instance Functor Tree where
  fmap f (Leaf x) = Leaf (f x)
  fmap f (Node l r) = Node (fmap f l) (fmap f r)
  
instance Traversable Tree where
  traverse g (Leaf x) = pure Leaf <*> g x
  traverse g (Node l r) = pure Node <*> traverse g l <*> traverse g r 

-- > traverse dec tree
-- Just (Node (Node (Leaf 2) (Leaf 3)) (Leaf 4))
-- > traverse dec tree'
-- Just (Node (Node (Leaf 2) (Leaf 3)) (Leaf 4))
-- > traverse dec $ Node (Leaf 1) (Leaf 2)
-- Just (Node (Leaf 0) (Leaf 1))
-- > traverse dec $ Node (Leaf 0) (Leaf 2)
-- Nothing

-- traverse g = sequence.fmap g

