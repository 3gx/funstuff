data Offer a = Present a
             | PercentDiscount Float
             | AbsoluteDiscount Float 
             | Restrict [a] (Offer a)
             | From Integer (Offer a) 
             | Until Integer (Offer a)
             | Extend Integer (Offer a)
             | Both (Offer a) (Offer a)
             | BetterOf (Offer a) (Offer a)
             | If (Expr a) (Offer a) (Offer a)
           deriving (Show)

data Expr a = AmountOf a | PriceOf a
            | TotalNumberProducts | TotalPrice 
            | IVal Integer | FVal Float
            | (Expr a) :+: (Expr a)
            | (Expr a) :*: (Expr a)
            | (Expr a) :<: (Expr a)
            | (Expr a) :<=: (Expr a)
            | (Expr a) :>: (Expr a)
            | (Expr a) :>=: (Expr a)
            | (Expr a) :&&: (Expr a)
            | (Expr a) :||: (Expr a)
            | Not (Expr a) 
  deriving (Show)

noOffer :: Offer a
noOffer = AbsoluteDiscount 0

v :: Offer String
v = Until 30 $ BetterOf (AbsoluteDiscount 10.0)
                          (Both (Present "ballon")
                          (If (TotalPrice :>: IVal 100) (PercentDiscount 5.0)
                                                         noOffer)) 

period :: Integer -> Integer -> Offer a -> Offer a
period f d o = From d (Until (f+d) o)


allOf :: [a] -> Offer a
allOf os = Restrict os  noOffer

v1 = period 3 5 (Both (Both (Present "ballon") (Present "choco muffin")) (PercentDiscount 10.0))


