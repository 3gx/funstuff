{-# LANGUAGE DataKinds, TypeFamilies, TemplateHaskell, QuasiQuotes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE UndecidableInstances #-}

--import Data.Singletons -- .TH
import Data.Singletons.TH hiding (Min, Compare, CompareSym0)

$(singletons [d|
  data Nat = Zero | Succ Nat
           deriving (Show, Eq)
  |])

$(promote [d|

  plus :: Nat -> Nat -> Nat
  plus Zero     y = y
  plus (Succ x) y = Succ (plus x y)

  min :: Nat -> Nat -> Nat
  min Zero     _        = Zero
  min _        Zero     = Zero
  min (Succ x) (Succ y) = Succ (min x y)
  |])

$(promote [d|
  data Range = Empty | Open Nat | Closed Nat Nat
  infinite :: Range
  infinite = Open Zero
  |])


$(promote [d|
  data Comparison = Less | Equal | Greater

  -- conflicts with Data.Singleton.TH.Compare & CompareSym0
  -- hiding when importing Data.Singleton.TH module
  compare :: Nat -> Nat -> Comparison
  compare Zero     Zero     = Equal
  compare Zero     (Succ _) = Less
  compare (Succ _) Zero     = Greater
  compare (Succ x) (Succ y) = compare x y

  restrictFrom :: Nat -> Range -> Range
  restrictFrom _ Empty    = Empty
  restrictFrom n (Open f) = restrictFrom1 n f (compare n f)
  restrictFrom n (Closed f t) = restrictFrom2 n f t (compare n f) (compare n t)

  restrictFrom1 :: Nat -> Nat -> Comparison -> Range
  restrictFrom1 n _ Greater = Open n
  restrictFrom1 _ f Equal   = Open f
  restrictFrom1 _ f Less    = Open f

  restrictFrom2 :: Nat -> Nat -> Nat -> Comparison -> Comparison -> Range
  restrictFrom2 _ _ _ Greater Greater = Empty
  restrictFrom2 _ _ _ Greater Equal   = Empty
  restrictFrom2 n _ t Greater Less    = Closed n t
  restrictFrom2 _ f t Equal   _       = Closed f t
  restrictFrom2 _ f t Less    _       = Closed f t

  restrictUntil :: Nat -> Range -> Range
  restrictUntil _ Empty    = Empty
  restrictUntil n (Open f) = restrictUntil1 n f (compare n f)
  restrictUntil n (Closed f t) = restrictUntil2 n f t (compare n f) (compare n t)

  restrictUntil1 :: Nat -> Nat -> Comparison -> Range
  restrictUntil1 n f Greater = Closed f n
  restrictUntil1 _ _ Equal   = Empty
  restrictUntil1 _ _ Less    = Empty

  restrictUntil2 :: Nat -> Nat -> Nat -> Comparison -> Comparison -> Range
  restrictUntil2 _ f t _       Greater = Closed f t
  restrictUntil2 _ f t _       Equal   = Closed f t
  restrictUntil2 n f _ Greater Less    = Closed f n
  restrictUntil2 _ _ _ Equal   Less    = Empty
  restrictUntil2 _ _ _ Less    Less    = Empty
  |])

data Offer a (r :: Range) where
  Present          :: a -> Offer a Infinite
  PercentDiscount  :: Float -> Offer a Infinite
  AbsoluteDisocunt :: Float -> Offer a Infinite
  From             :: SNat n -> Offer a d -> Offer a (RestrictFrom n d)
  Until            :: SNat n -> Offer a d -> Offer a (RestrictUntil n d)

toNat :: SNat n -> Nat
toNat SZero = Zero
toNat (SSucc n) = Succ (toNat n)

printDateRestriction :: Offer a r -> String
printDateRestriction (From n _)  = "From " ++ show (toNat n)
printDateRestriction (Until n _) = "Until" ++ show (toNat n)
printDateRestriction _           = "No date restriction"

zero :: SNat Zero
zero = sing   -- results in SZero
one :: SNat (Succ Zero)
one = sing    -- results in SSucc SZero
two :: SNat (Succ (Succ Zero))
two = sing    -- results in SSucc (SSucc SZero)
three :: SNat (Succ (Succ (Succ Zero)))
three = sing  -- results in SSucc (SSucc (SSucc SZero))

