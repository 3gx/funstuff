{-# LANGUAGE GADTs,  
             MultiParamTypeClasses, 
             TypeFamilies
#-}

data Offer a = Present a
             | PercentDiscount Float
             | AbsoluteDiscount Float 
             | Restrict [a] (Offer a)
             | From Integer (Offer a) 
             | Until Integer (Offer a)
             | Extend Integer (Offer a)
             | Both (Offer a) (Offer a)
             | BetterOf (Offer a) (Offer a)
--             | If (Expr a) (Offer a) (Offer a)
           deriving (Show)

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
 
  
noOffer :: Offer a
noOffer = AbsoluteDiscount 0

{-
v :: Offer String
v = Until 30 $ BetterOf (AbsoluteDiscount 10.0)
                          (Both (Present "ballon")
                          (If (TotalPrice :>: IVal 100) (PercentDiscount 5.0)
                                                         noOffer)) 
-}

period :: Integer -> Integer -> Offer a -> Offer a
period f d o = From d (Until (f+d) o)


allOf :: [a] -> Offer a
allOf os = Restrict os  noOffer

v1 = period 3 5 (Both (Both (Present "ballon") (Present "choco muffin")) (PercentDiscount 10.0))

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

