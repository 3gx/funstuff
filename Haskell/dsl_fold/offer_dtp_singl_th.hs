{-# LANGUAGE DataKinds, TypeFamilies, TemplateHaskell, QuasiQuotes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE UndecidableInstances #-}

--import Data.Singletons -- .TH
import Data.Singletons.TH hiding (Min)

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


{-
$(singletons [d|
  data Nat = Zero | Succ Nat
           deriving (Show, Eq)
  |])
-}

