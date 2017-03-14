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

class PMonad m where
  unit :: a -> m p p a
  bind :: m p q a -> (a -> m q r b) -> m p r b

-- start phantom
data Nil
data Cons l s

data Locked
data Unlocked
-- end phantom

newtype LockM p q a = LockM { unLockM :: IO a }

instance PMonad LockM where
  unit x   = LockM (return x)
  bind m k = LockM (unLockM m >>= unLockM . k)

lput :: String -> LockM p p ()
lput = LockM . putStrLn

type family Get n p
type instance Get Zero (Cons e p) = e
type instance Get (Succ n) (Cons e p) = Get n p

type family Set n e' p
type instance Set Zero e' (Cons e p) = Cons e' p
type instance Set (Succ n) e' (Cons e p) = Cons e (Set n e' p)

newtype Lock n = Lock Int deriving Show

mkLock :: forall n . Nat n => Lock n
mkLock = Lock (toInt (undefined :: n))

lock1 = mkLock :: Lock One
lock4 = mkLock :: Lock Four

acquire :: (Get n p ~ Unlocked) => Lock n -> LockM p (Set n Locked p) ()
acquire l = LockM (putStrLn ("acquire" ++ show l))

release :: (Get n p ~ Locked) => Lock n -> LockM p (Set n Unlocked p) ()
release l = LockM (putStrLn ("release" ++ show l))

type ThreeLocks = Cons Unlocked (Cons Unlocked (Cons Unlocked Nil))
run :: LockM ThreeLocks ThreeLocks a -> IO a
run = unLockM 

with1 a = acquire lock1 `bind` \_ ->
          a `bind` \x ->
          release lock1 `bind` \_ ->
          unit x

with02 a = acquire_locks `bind` \_ ->
          a `bind` \x ->
          release_locks `bind` \_ ->
          unit x
 where
 -- We acquire and release in the same order. Not a LIFO
 acquire_locks = acquire (mkLock::Lock Zero) `bind` \_ -> 
     acquire (mkLock::Lock Two)
 release_locks = release (mkLock::Lock Zero) `bind` \_ ->
     release (mkLock::Lock Two)

with01 a = acquire_locks `bind` \_ ->
          a `bind` \x ->
          release_locks `bind` \_ ->
          unit x
 where
 -- We acquire and release in the same order. Not a LIFO
 acquire_locks = acquire (mkLock::Lock One) `bind` \_ -> 
     acquire (mkLock::Lock Two)
 release_locks = release (mkLock::Lock Two) `bind` \_ ->
     release (mkLock::Lock One)

critical1 :: (Get One p ~ Locked) => LockM p p ()
critical1 = LockM (putStrLn "Critical section 1") 

-- Appendix B.2
---------------

type family Plus m n
type instance Plus Zero n = n
type instance Plus (Succ m) n = Succ (Plus m n)

plus :: m -> n -> Plus m n
plus = undefined

tplus = plus (undefined::Two) (undefined::Three)
tplus' x = if True then plus x (undefined::One) else tplus


main = do
  print $ toInt (undefined :: Two)
  print $ toInt (undefined :: Eight)  
  print "--------"
  run (with1 (lput "hello"))
  print "--------"
  run (with02 (lput "hello again"))
  print "--------"
  run (with1 critical1)
  print "--------"
  run (with01 critical1)

