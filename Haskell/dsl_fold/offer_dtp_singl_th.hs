{-# LANGUAGE DataKinds, TypeFamilies, TemplateHaskell, QuasiQuotes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE UndecidableInstances #-}

--import Data.Singletons -- .TH
import Data.Singletons.TH hiding (Min, Compare, CompareSym0)

$(promote [d|
  data Nat = Zero | Succ Nat
           deriving (Show, Eq)
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
  |])

{-
$(singletons [d|
  data Nat = Zero | Succ Nat
           deriving (Show, Eq)
  |])
-}

