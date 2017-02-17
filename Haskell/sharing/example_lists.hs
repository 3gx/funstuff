{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}

import Data.Reify

newtype Mu a = In (a (Mu a))

data List a b = Cons a b | Nil deriving Show

type MyList a = Mu (List a)

g = In (Cons 1 (In (Cons 2 (In (Cons 3 (In Nil))))))

instance (Traversable a) => MuRef (Mu a) where
  type DeRef (Mu a) = a
  mapDeRef f (In a) = traverse f a 

instance Functor (List a) where
  fmap _ Nil = Nil
  fmap f (Cons a b) = Cons a (f b)

instance Foldable (List a) where
  foldMap f Nil = mempty
  foldMap f (Cons a b) = f b

instance Traversable (List a) where
  traverse f (Cons a b) = Cons a <$> f b
  traverse f Nil = pure Nil

instance MuRef [a] where
  type DeRef [a] = List a
  mapDeRef f (x:xs) = Cons x <$> f xs
  mapDeRef f []     = pure Nil

main = do
  let xs = In (Cons 98 (In (Cons 101 xs)))
  reifyGraph xs >>= print
  let xs = 99:100:xs
  reifyGraph xs >>= print
