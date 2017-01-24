{-# LANGUAGE RankNTypes #-}
-- https://skillsmatter.com/skillscasts/4251-lenses-compositional-data-access-and-manipulation#video


data LensR s a = L { viewR :: s -> a, setR :: a -> s -> s }


{-
  composeL :: Lens s1 s2 -> Lens s2 a -> Lens s1 a 
  
  setPostcode :: String -> Person -> Person
  setPostcode pc p = set (ladder `composeL` lpostcode) pc p

  composeL (L v1 u1) (L v2 u2) = 
     L (\s -> v2 (v1 s))
       (\a s -> u1 (u2 a (v1 s)) s)
  
-}
 
composeL :: LensR s1 s2 -> LensR s2 a -> LensR s1 a 
-- LensR s1 s2 = L { v1 :: s1 -> s2, 
--                   s1 :: s2 -> s1 -> s1 }
-- LensR s2 a  = L { v2 :: s2 -> a,
--                   s2 :: a  -> s2 -> s2 }
composeL (L v1 s1) (L v2 s2) = 
   L (\s -> v2 (v1 s))
     (\a s -> s1 (s2 a (v1 s)) s)

--data KK = K { vK :: String , sK :: Int } deriving Show
----cc (K v1 s1) (K v2 s2) = v1 ++ " - " ++ v2

type Lens' s a = forall f. Functor f => (a -> f a) -> s -> f s


newtype Identity a = Identity a deriving Show
-- Id :: a -> Identity a

instance Functor Identity where
  fmap f (Identity x) = Identity (f x)

runIdentity :: Identity s -> s
runIdentity (Identity x) = x

const' :: a -> b -> a
const' x _ = x

const'' :: a -> b -> (a,b)
const'' x y = (x,y)

set :: Lens' s a -> a -> s -> s
-- set :: Functor f => ((a -> f a) -> s -> f s)  -> (a -> s -> s)
{- set :: (a -> f a) -> s -> f s -> (a -> s -> s)
   set       ln         a     s 
-}
{-
set ln x s = runIdentity (ln set_fld s)
      where set_fld _ = Identity x
-}
--set ln x = runIdentity . ln (Identity . const' x)
--set ln x s = runIdentity (ln set_fld s)
 --     where set_fld _ = Identity x

--set ln x s = runIdentity (ln set_fld s)
--     where set_fld _ = Identity x

--set ln x s = runIdentity (ln (\_ -> Identity x) s)
--set ln x = runIdentity . ln (Identity . const' x)
--                        Lens (a -> f a) -> s -> f s
--                             f = Identity
--                             returns Identity s

--set ln x = runIdentity . ln (Identity . (const' x))
over :: Lens' s a -> (a -> a) -> s -> s
over ln f = runIdentity . ln (Identity . f) 
set ln = (over ln) . const'

newtype Const v ignore_type = Const v deriving Show

getConst :: Const v a -> v
getConst (Const x) = x

instance Functor (Const v) where
  fmap f (Const x) = Const x

--view :: Lens' s a -> (s -> a)
view :: Lens' s a -> s -> a
--view ln s = getConst (ln Const s)
view ln = getConst . ln Const

view' :: Lens' s a -> s -> Const a s
view' ln = ln Const

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
--name :: Functor f => (String -> f String) -> Person -> f Person
-- name elt_fn (P n s) = 
--    fmap (\n' -> P n' s) (elt_fn n)
--      |_______|_______________|____ Functor f => (a->b) -> f a -> f b
--              |_______________|____ String -> Person
--              |               |____ f String 
--              |___ this function is like a data structure with a hole in it! 
-- (<$>) = fmap
name elt_fn (P n s) = (\n' -> P n' s) <$> (elt_fn n)

salary :: Lens' Person Int
salary elt_fn (P n s) = (\s' -> P n s') <$> (elt_fn s)

fred = P { _name = "Fred", _salary = 100 }
-- Using lens
-- > let fred = P { _name = "Fred", _salary = 100 }
-- > view name fred
-- "Fred"
-- > set name "Bill" fred
-- P {_name = "Bill", _salary = 100}
--
-- How on earth does this work?
--
-- view name fred 
-- = view name (P {_name = "Fred", _salary = 100} )
-- = getConst (name Const (P {_name = "Fred", _salary = 100}) )
-- = getConst ( fmap (\n' -> P n' 100) (Const "Fred"))
-- = getConst (Const "Fred") 
-- = "Fred"

-- ============================
--  Composing and Using Lenses
-- ============================



