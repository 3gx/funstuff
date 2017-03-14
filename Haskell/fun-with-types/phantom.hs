{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- {-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, EmptyDataDecls, ScopedTypeVariables #-}

-- 5 Fun with phantom types
---------------------------

data Zero
data Succ n

type One   = Succ Zero
type Two   = Succ One
type Three = Succ Two
type Four  = Succ Three
type Five  = Succ Four
type Six   = Succ Five
type Seven = Succ Six
type Eight = Succ Seven
type Nine  = Succ Eight
type Ten   = Succ Nine

class Nat n where
  toInt :: n -> Int

instance Nat Zero where
  toInt _ = 0

instance (Nat n) => Nat (Succ n) where
  toInt _ = 1 + toInt (undefined :: n)

newtype Pointer n = MkPointer Int deriving Show
newtype Offset  n = MkOffset  Int deriving Show

multiple :: forall n. (Nat n) => Int -> Offset n
multiple i = MkOffset ( i * toInt (undefined :: n))

add :: Pointer m -> Offset n -> Pointer (GCD Zero m n)
add (MkPointer x) (MkOffset y) = MkPointer (x+y)

type family GCD d m n
type instance GCD d Zero Zero = d
type instance GCD d (Succ m) (Succ n) = GCD (Succ d) m n 
type instance GCD Zero (Succ m) Zero = Succ m
type instance GCD (Succ d) (Succ m) Zero = GCD (Succ Zero) d m
type instance GCD Zero Zero (Succ n) = Succ n
type instance GCD (Succ d) Zero (Succ n) = GCD (Succ Zero) d n

fetch32 :: (GCD Zero n Four ~ Four) => Pointer n -> IO ()
fetch32 = undefined

p1 :: Pointer Eight
p1 = MkPointer 8
o1 :: Offset Four
o1 = multiple 1
p2 = add p1 o1

g = fetch32 p2 -- compiles because p2 ha Pointer Four

-- 5.2 Tracking state & control in a parametrised monad
-------------------------------------------------------

main = do
  print $ toInt (undefined :: Two)
  print $ toInt (undefined :: Eight)  

