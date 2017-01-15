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
-- closed type family
type family Plus x y where
  Plus Zero x = x
  Plus (Succ x) y = Succ (Plus x y)

-- closed type family
type family Min x y where
  Min (Succ x) (Succ y) = Succ (Min x y)
  Min _ _ = Zero

data Offer a p where
  Present          :: a -> Offer a (Succ Zero)
  PercentDiscount  :: Float -> Offer a Zero
  AbsoluteDiscount :: Float -> Offer a Zero
  Both             :: Offer a p -> Offer a q -> Offer a (Plus p q)
  Restrict         :: Vect (Succ n) a -> Offer a p -> Offer a (Min (Succ n) p)

data TimeMachine = TimeMachine { model :: String } deriving Show
data TimeMachineOps = Travel Integer | Park deriving Show

data Book = Book { title :: String, author :: String, rating :: Integer } deriving Show
data BookOps = Read | Highlight | WriteCritique deriving Show

{-
-- open type family
type family Operation x

type instance Operation TimeMachine = TimeMachineOps
type instance Operation Book        = BookOps
-}

class Product p where
  type Operation p :: *
  price :: p -> Float
  perform :: p -> Operation p -> String
  testOperation :: p -> Operation p

instance Product TimeMachine where
  type Operation TimeMachine = TimeMachineOps
  price _ = 1000.0
  perform (TimeMachine m) (Travel y) = "Traveling to " ++ show y ++ " with " ++ m
  perform (TimeMachine m) Park       = "Parking time machine " ++ m
  testOperation _                    = Travel 0

totalAmount :: Product p => [p] -> Float
totalAmount = foldr (+) 0.0 . map price

performTest :: Product p => p -> String
performTest p = perform p $ testOperation p
