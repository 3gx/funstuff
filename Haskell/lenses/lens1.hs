{-# LANGUAGE RankNTypes #-}
-- https://skillsmatter.com/skillscasts/4251-lenses-compositional-data-access-and-manipulation#video
type Lens' s a = forall f. Functor f => (a -> f a) -> s -> f s

data LensR s a = L { viewR :: s -> a, setR :: a -> s -> s }

newtype Identity a = Identity a
-- Id :: a -> Identity a


runIdentity :: Identity s -> s
runIdentity (Identity x) = x

instance Functor Identity where
  fmap f (Identity x) = Identity (f x)

const' :: a -> b -> a
const' x _ = x

set :: Lens' s a -> (a -> s -> s)
{-
set ln x s = runIdentity (ln set_fld s)
      where set_fld _ = Identity x
-}
set ln x = runIdentity . ln (Identity . const' x)

over :: Lens' s a -> (a -> a) -> s -> s
over ln f = runIdentity . ln (Identity . f) 

newtype Const v a = Const v

getConst :: Const v a -> v
getConst (Const x) = x

instance Functor (Const v) where
  fmap f (Const x) = Const x

view :: Lens' s a -> (s -> a)
--view ln s = getConst (ln Const s)
view ln = getConst . ln Const

lensToLensR :: Lens' s a -> LensR s a
lensToLensR ln = L { viewR = view ln, setR = set ln }

--  Homework
-- ==========

--  * implement:
-- lensRToLens :: LensR s a -> Lens' s a


--  Make a Lens
-- =============

data Person = P { _name :: String, _salary :: Int } deriving Show

name :: Lens' Person String
-- name elt_fn (P n s) = fmap (\n' -> P n' s) (elt_fn n)
-- (<$>) = fmap
name elt_fn (P n s) = (\n' -> P n' s) <$> (elt_fn n)

-- Using lens
-- > let fred = P { _name = "Fred", _salary = 100 }
-- > view name fred
-- "Fred"
-- > set name "Bill" fred
-- P {_name = "Bill", _salary = 100}
--
