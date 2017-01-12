{-# LANGUAGE UndecidableInstances, RankNTypes #-}
import Data.Monoid
import Control.Monad

data ExprF a = Const Int
             | Add a a
             | Mul a a  

data Fix f = In ( f (Fix f))

-- requires UndecidableInstances
instance Show (f (Fix f)) => Show (Fix f) where
  show (In i) = "(In $ " ++ show i ++ ")"

instance Show a => Show (ExprF a)where
  show (Const i) = "(Const " ++ show i ++")"
  show (Add x y) = "(Add " ++ show x ++ show y ++ ")"
  show (Mul x y) = "(Mul " ++ show x ++ show y ++ ")"

testExpr = In $ (In $ (In $ Const 2) `Add` 
            (In $ Const 3)) `Mul` (In $ Const 4)

g = Const 3
k = Mul g (Add (Const 5) g)

-- requires RankNTypes
newtype List a = List (forall b. Monoid b => (a->b)->b)

-- liftFree :: Functor f => fa -> Free f a
-- foldFree :: Functor f => (f r -> r) -> Free f r -> r

{- -- defined in Prelude
class Monoid m where
  mempty :: m
  mappend :: m -> m -> m
--}

{- -- defined in GHC.Base
instance Monoid [t] where
  mempty = []
  mappend = (++)
--}

data Free f a = Pure a | Roll (f (Free f a))

instance Functor f => Functor (Free f) where
  fmap f (Pure a) = Pure (f a)
  fmap f (Roll x) = Roll (fmap (fmap f) x)

concatFree :: Functor f => Free f (Free f a) -> Free f a
concatFree (Pure x) = x
concatFree (Roll y) = Roll (fmap concatFree y)

  
  
instance Functor f => Applicative (Free f) where
  pure = Pure
  Pure f <*> a =  fmap f a
  Roll f <*> a = Roll $ fmap (<*> a) f

instance Functor f => Monad (Free f) where
  return = Pure 
  x >>= f = concatFree (fmap f x)

-- this is essentially the same as \x -> [x]
liftFree :: Functor f => f a -> Free f a
liftFree x = Roll (fmap Pure x)

-- this is essentially the same as folding a list
foldFree :: Functor f => (f r -> r) -> Free f r -> r
foldFree _ (Pure a) = a
foldFree f (Roll x) = f (fmap (foldFree f) x)
