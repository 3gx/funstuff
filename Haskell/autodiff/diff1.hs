-- Dual is because there are two
data Dual a  = Dual a a deriving (Eq, Read, Show)

-- if "a" is a "Num", then "Dual a" is also a "Num"
instance Num a => Num (Dual a) where
  (Dual u u') + (Dual v v') = Dual (u + v) (u' + v')
  (Dual u u') * (Dual v v') = Dual (u * v) (u * v' + u' * v)
  (Dual u u') - (Dual v v') = Dual (u - v) (u' - v')
  abs (Dual u u')           = Dual (abs u) (u' * (signum u))
  signum (Dual u u')        = Dual (signum u) 0
  fromInteger n             = Dual (fromInteger n) 0

-- `Fractional a` implies `Fractional (Dual a)`
instance Fractional a => Fractional (Dual a) where
  (Dual u u') / (Dual v v') = Dual (u / v) ((u' * v - u * v') / (v * v))
  fromRational n            = Dual (fromRational n) 0

-- `Floating a` implies `Floating (Dual a)`
instance (Eq a, Floating a) => Floating (Dual a) where
  pi                = Dual pi 0
  exp (Dual u u')   = Dual (exp u) (u' * (exp u))
  log (Dual u u')   = Dual (log u) (u' / u)
  sqrt (Dual u u')  = Dual (sqrt u) (u' / (2 * sqrt u))
  sin (Dual u u')   = Dual (sin u) (u' * (cos u))
  cos (Dual u u')   = Dual (cos u) (-1 * u' * (sin u))
  tan (Dual u u')   = Dual (tan u) (1 / ((cos u) ** 2))
  asin (Dual u u')  = Dual (asin u) (u' / (sqrt(1 - (u ** 2))))
  acos (Dual u u')  = Dual (acos u) ((- 1) * u' / (sqrt(1 - (u ** 2))))
  atan (Dual u u')  = Dual (atan u) (u' / (1 + (u ** 2)))
  sinh (Dual u u')  = Dual (sinh u) (u' * cosh u)
  cosh (Dual u u')  = Dual (cosh u) (u' * sinh u)
  tanh (Dual u u')  = Dual (tanh u) (u' * (1 - ((tanh u) ** 2)))
  asinh (Dual u u') = Dual (asinh u) (u' / (sqrt(1 + (u ** 2))))
  acosh (Dual u u') = Dual (acosh u) ((u' / (sqrt((u ** 2) - 1))))
  atanh (Dual u u') = Dual (atanh u) (u' / (1 - (u ** 2)))
  (Dual u u') ** (Dual n 0)
    = Dual (u ** n) (u' * n * u ** (n - 1))
  (Dual a 0) ** (Dual v v')
    = Dual (a ** v) (v' * log a * a ** v)
  (Dual u u') ** (Dual v v')
    = Dual (u ** v) ((u ** v) * (v' * (log u) + (v * u' / u)))
  logBase (Dual u u') (Dual v v')
    = Dual (logBase u v) (((log v) * u' / u - (log u) * v' / v) / ((log u) ** 2))

-- `Ord a` implies `Ord (Dual a)`
instance Ord a => Ord (Dual a) where
  (Dual x _) <= (Dual y _) = x <= y

-- Extract the derivative `x'` from `Dual _ x'`
diffDual :: Dual a -> a
diffDual (Dual _ x') = x'

-- Differentiate a single-variable function `f`
d :: Num a => (Dual a -> Dual c) -> a -> c
d f x = diffDual . f $ Dual x 1

fib x
  | x < 0 = 0
  | x <= 2 = x
  | otherwise = fib(x - 1) + fib(x - 2)


g x y = x ** 2 - 3 * x * y + 2 * y ** 2

infixl 4 :+:
infixl 5 :*:

-- Type for representing algebraic expressions
data Expr a
  = Var Char
  | Const a
  | (Expr a) :+: (Expr a)
  | (Expr a) :*: (Expr a)
  deriving (Eq, Read, Show)

-- `Num a` implies `Num (Expr a)`
instance Num a => Num (Expr a) where
  u + v         = u :+: v
  u * v         = u :*: v
  u - v         = u :+: Const (-1) :*: v
  fromInteger n = Const $ fromInteger n
  abs u         = undefined
  signum u      = undefined

-- some stock `Expr a`s to play around with
-- remember to only use `(+)`, `(-)`, `(*)`, and `fromInteger`
f1 = 3 * Var 'x' + 4 -- represents `\x -> 3 * x + 4`
g1 = 4 - Var 'x' * Var 'x' -- represents `\x -> 4 - x * x`
fx1 = Var 'x' * Var 'x' - 3 * Var 'x'
fx x = x *x - 3 *x
fxy x y = x*x + 3*x*y

-- `d` will find the derivative of a haskell function. We need a way to
-- turn an `Expr a` into a haskell function. `applyExpr` helps us do that.

-- applies `expr` to argument `x`
applyExpr :: Expr a -> a -> a
applyExpr expr x = undefined -- exercise left to the reader ;-)

deriv :: (Num a ) => Expr a -> Expr a
deriv (Var x ) = Const 1
deriv (Const c) = Const 0
deriv (u :+: v) = deriv u + deriv v
deriv (u :*: v) = u  * (deriv v) + (deriv u) * v

main = do
  let f x = x + 1/x
  print $ f  $ Dual 3 1
  let f x = sin(x)*exp(x)
  print $ f $ Dual 3 1
  let f x = x ** x
  print $ f $ Dual 3 1
  print $ f $ Dual 0 1
  let f x = x ** 2 
  print $ f $ Dual 3 1
  print $ f $ Dual 0 1
  let g = d f
  print $ g 3
  print $ g 0
  let h x = foldr (\_ z -> sin (x + z)) x [1..100]
  print $ h 4
  let dh = d h
  print $ dh 4
  print $ fib 10
  print $ fib 11
  print $ d fib 10
  print $ (fib 10)  + (d fib 10)
--  let k = g (Dual 2 1) (Dual 1 0) :: Dual Double
--  print k
  print $ (f1, deriv f1)
  print $ (g1, deriv g1)
  print $ (fx1, deriv fx1)
  print $ d (\x -> 3 * x*x + 4) (Var 'x')
  print $ d fx (Var 'x')
  print $ fx (Dual (Var 'x') (Const 1))
  print $ fxy (Dual (Var 'x') (Const 1)) (Dual (Var 'y') (Const 0))


