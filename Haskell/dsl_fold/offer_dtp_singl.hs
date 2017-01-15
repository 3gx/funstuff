{-# LANGUAGE GADTs
            ,DataKinds 
#-}

data Nat = Zero | Succ Nat 

data Vect n a where
  -- requires DataKinds
  VNil :: Vect Zero a
  VCons :: a -> Vect n a -> Vect (Succ n) a
