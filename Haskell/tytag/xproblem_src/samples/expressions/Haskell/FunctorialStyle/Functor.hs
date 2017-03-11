module Functor where


-- Fixed point on functors

data Fix f = In (f (Fix f))
unIn (In x) = x


-- Sum of functors

data SumF f g x = LeftF (f x) | RightF (g x)

instance (Functor f, Functor g) => Functor (SumF f g) 
 where
  fmap f (LeftF x)  = LeftF  $ fmap f x
  fmap f (RightF x) = RightF $ fmap f x

bimap f g (LeftF x)  = f x
bimap f g (RightF x) = g x


-- Folds over functors

fold :: (Functor f) => (f a -> a) -> Fix f -> a
fold f = f . fmap (fold f) . unIn
