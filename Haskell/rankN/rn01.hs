{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}

import Data.Char

data Some  :: * -> * where
    SomeInt  :: Int -> Some Int
    SomeChar :: Char -> Some Char
    Anything :: a -> Some a

unSome :: Some a -> a
unSome (SomeInt x) = x + 3
unSome (SomeChar c) = toLower c
unSome (Anything x) = x

newtype SomeC a =
    SomeC {
      runSomeC ::
          forall r.
          ((a ~ Int) => Int -> r) ->
          ((a ~ Char) => Char -> r) ->
          (a -> r) ->
          r
    }

type SomeInt' = SomeC Int
