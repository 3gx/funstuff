-- http://userpages.uni-koblenz.de/~laemmel/TheEagle/resources/xproblem2.html
-- spin-offs from the expression problem

{-# LANGUAGE DatatypeContexts #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-} 
{-# LANGUAGE OverlappingInstances #-} 
{-# LANGUAGE ScopedTypeVariables #-} 

import Data.Typeable

data Lit = Lit Int deriving Show

class Exp x 
instance Exp Lit

-- requires ExistentialQuantification
data AnyExp = forall x. Show x => AnyExp x
data (Exp l, Exp r) => Add l r = Add l r deriving Show

instance Show AnyExp
 where
  show (AnyExp x) = show x

class ShowType x 
 where
  t :: x -> String

instance Typeable x => ShowType x
 where
  t x = show $ typeOf x

instance (ShowType x, ShowType y) => ShowType (x -> y)
 where
  t _ = "(" ++ t (undefined::x) ++ " -> " ++ t (undefined::y) ++ ")"

{-
instance ShowType Lit where
    t _ = "Exp"

instance (Exp x, Exp y) => ShowType (Add x y) where
    t _ = "Exp"

instance Exp x => ShowType (Neg x) where
    t _ = "Exp"
-}


-- requires DatatypeContextx
data Exp x => Neg x = Neg x deriving Show

main = do
-- > [Lit 1, Neg (Lit 3)]
-- type-error
 print $  [AnyExp $ Lit 1, AnyExp $ Neg (Lit 3)]
-- > :t Add (Lit 2) (Lit 3)
-- Add (Lit 2) (Lit 3) :: Add Lit Lit
 print $  [AnyExp $ Lit 1, AnyExp $ Neg (Lit 3), AnyExp $ Add (Lit 2) (Lit 2)]
 print $ t $ Add (Lit 2) (Lit 3) 

