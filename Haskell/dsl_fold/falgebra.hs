{-data Expr = Const' Int
          | Add' Expr Expr
          | Mul' Expr Expr deriving (Show)
-}

data ExprF a = Const Int
             | Add a a
             | Mul a a  deriving (Show)

type Expr = Fix ExprF

newtype Fix f = In (f (Fix f))


--In' :: f (Fix f) -> Fix f


val :: Fix ExprF
val = In (Const 12)

testExpr = In $ (In $ (In $ Const 2) `Add` 
            (In $ Const 3)) `Mul` (In $ Const 4)

instance Functor ExprF where
  fmap eval (Const i) = Const i
  fmap eval (left `Add` right) = (eval left) `Add` (eval right)
  fmap eval (left `Mul` right) = (eval left) `Mul` (eval right)

{-
alg :: ExprF Int -> Int
alg (Const i) = i
alg (x `Add` y) = x + y
alg (x `Mul` y) = x * y

pretty :: ExprF String -> String
pretty (Const i) = show i
pretty (x `Add` y) = "(" ++ x ++ " + " ++ y ++ ")"
pretty (x `Mul` y) = "(" ++ x ++ " * " ++ y ++ ")"
-}

type Algebra f a = f a -> a
type SimpleA = Algebra ExprF Int

alg :: SimpleA
alg (Const i) = i
alg (x `Add` y) = x + y
alg (x `Mul` y) = x * y

testExpr' = (Const 3) `Mul` (Const 4)

-- k = fmap alg


main = do
  print "123"
  print $ fmap alg testExpr'
