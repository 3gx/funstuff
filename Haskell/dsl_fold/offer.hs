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
