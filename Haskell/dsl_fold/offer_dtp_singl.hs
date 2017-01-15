{-# LANGUAGE GADTs
            ,DataKinds 
            ,TypeFamilies
#-}

data Nat = Zero | Succ Nat 

data Vect (n::Nat) a where
  -- requires DataKinds
  VNil :: Vect Zero a
  VCons :: a -> Vect n a -> Vect (Succ n) a


type family Plus' x y where
  Plus' Zero x = x
  Plus' (Succ x) y = Succ (Plus' x y)

type family Plus (x::Nat) (y::Nat) :: Nat
type instance Plus Zero x = x
type instance Plus (Succ x) y = Succ (Plus x y)

data Offer a (p::Nat) where
  Present          :: a -> Offer a (Succ Zero)
  PercentDiscount  :: Float -> Offer a Zero
  AbsoluteDiscount :: Float -> Offer a Zero
  Both             :: Offer a p -> Offer a q -> Offer a (Plus p q)
--  Restrict         :: Vect (Succ n) a -> Offer a p -> Offer a (Min (Succ n) p)
