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

instance Num IntE where
  (+) = typ2 Add
  (*) = typ2 Mul
  negate = typ1 Negate
  abs = typ1 Abs
  fromInteger = E . LitInt . fromInteger

expr1 = (3+4)::IntE
