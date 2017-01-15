{-# LANGUAGE GADTs,  
             MultiParamTypeClasses, 
             TypeFamilies
#-}


data Expr a r where
  AmountOf            :: a -> Expr a Integer
  PriceOf             :: a -> Expr a Float
  TotalNumberProducts :: Expr a Integer
  TotalPrice          :: Expr a Float
  Val                 :: Num n => n -> Expr a n
  (:+:)               :: Num n => Expr a n -> Expr a n -> Expr a n
  (:*:)               :: Num n => Expr a n -> Expr a n -> Expr a n
  (:<:)               :: Num n => Expr a n -> Expr a n -> Expr a Bool
  (:<=:)              :: Num n => Expr a n -> Expr a n -> Expr a Bool
  (:>:)               :: Num n => Expr a n -> Expr a n -> Expr a Bool
  (:>=:)              :: Num n => Expr a n -> Expr a n -> Expr a Bool
  (:&&:)              :: Expr a Bool -> Expr a Bool -> Expr a Bool
  (:||:)              :: Expr a Bool -> Expr a Bool -> Expr a Bool
  Not                 :: Expr a Bool -> Expr a Bool 
 
  
-- oops, illegal expression, but it still type checks
{- doesn't type check with GADTs
incorrectExpression :: Expr Char
incorrectExpression = TotalPrice :||: (TotalNumberProducts :<: PriceOf 'a')
-}

interpretExpr :: Eq a => Expr a t -> [(a,Float)] -> t
interpretExpr (e1 :+: e2) list = interpretExpr e1 list + interpretExpr e2 list

data AllowEverything
data AllowProducts
data AllowPurchases

data Person = Person  { firstName :: String }

data User r where
  Admin        :: Person -> User AllowEverything
  StoreManager :: Person -> User AllowEverything
  StorePerson  :: Person -> User AllowProducts
  Client       :: Person -> User AllowPurchases

{-
changePurchaseFinalPrice :: User AllowEverything -> Purchase -> Float -> Purchase
changePurchaseFinalPrice = ... 
-}

data Zero
data Succ n

data Vect n a where
  VNil :: Vect Zero a
  VCons :: a -> Vect n a -> Vect (Succ n) a


-- requires TypeFamilies
type family Plus x y where
  Plus Zero x = x
  Plus (Succ x) y = Succ (Plus x y)

type family Min x y where
  Min (Succ x) (Succ y) = Succ (Min x y)
  Min _ _ = Zero

data Offer a p where
  Present          :: a -> Offer a (Succ Zero)
  PercentDiscount  :: Float -> Offer a Zero
  AbsoluteDiscount :: Float -> Offer a Zero
  Both             :: Offer a p -> Offer a q -> Offer a (Plus p q)
  Restrict         :: Vect (Succ n) a -> Offer a p -> Offer a (Min (Succ n) p)

