{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
type Id = String

data Type = Vec Int 
          | Matrix Int Int 
          | Output
    deriving (Eq, Show)

data DExp = 
    LitInt Int | LitFloat Float | LitBool Bool
  | Var Id Type | Led Id Type DExp | If DExp DExp DExp
  | Add DExp DExp |  Mul DExp DExp | Sub DExp DExp | Div DExp DExp
  | Sin DExp | Cos DExp | Sqrt DExp | Negate DExp | Abs DExp
  | Or DExp DExp | And DExp DExp | Not DExp
 deriving Show


data Exp a = E DExp  deriving Show


type BoolE  = Exp Bool
type IntE   = Exp Int
type FloatE = Exp Float

typ1 :: (DExp -> DExp) -> Exp a -> Exp b
typ1 f (E e1) = E (f e1)

typ2 :: (DExp -> DExp -> DExp) -> Exp a -> Exp b -> Exp c
typ2 f (E e1) (E e2) = E (f e1 e2)

typ3 :: (DExp -> DExp -> DExp -> DExp) -> Exp a -> Exp b -> Exp c -> Exp d
typ3 f (E e1) (E e2) (E e3) = E (f e1 e2 e3)

instance Num IntE where
  (+) = typ2 Add
  (*) = typ2 Mul
  negate = typ1 Negate
  abs = typ1 Abs
  fromInteger = E . LitInt . fromInteger
  signum = undefined

expr1 = (3+4)::IntE

ifD :: DExp -> DExp -> DExp -> DExp
ifD (LitBool  True) a b = a
ifD (LitBool False) a b = b
ifD (Not c) a b = ifD c b a
ifD (If c d e) a b = ifD c (ifD d a b) (ifD e a b)
--ifD c a b = ifZ c a b

-- type-safe smart ctor
(&&*) :: BoolE -> BoolE -> BoolE
(&&*) = typ2 andD

-- non-type-safe smart ctor
andD :: DExp -> DExp -> DExp

-- constant folding
andD (LitBool a) (LitBool b) = LitBool (a && b)

-- if-floating
andD (If c a b) e2 = ifD c (andD a e2) (andD b e2)

class Syntactic a where
  ifE :: BoolE -> a -> a -> a

instance Syntactic (Exp a) where
  ifE = typ3 ifD

