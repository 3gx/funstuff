--import Data.Fix  as Fix
{-data Expr = Const' Int
          | Add' Expr Expr
          | Mul' Expr Expr deriving (Show)
-}

data ExprF a = Const Int
             | Add a a
             | Mul a a  deriving (Show)

type Expr = Fix ExprF

newtype Fix f = In ( f (Fix f))
--newtype Fix' f = In { unFix' :: f (Fix' f)}
--newtype Fix' f = In' { unFix' :: f (Fix' f) }

--instance Show f => Show (Fix f) where
 -- show (In (f (Fix f))) = show f ++ " (Fix " ++ show f ++ ") "

--instance Show (f (Fix f)) => Show (Fix f) where
----  show x = "(Fix " + show (unFix' x) ++ ")"


val :: Fix ExprF
val = In (Const 12)

testExpr = In $ (In $ (In $ Const 2) `Add` 
            (In $ Const 3)) `Mul` (In $ Const 4)


-- Altnrnatively use {-# LANGUAGE DeriveFunctor #-}, see falgebra_01.hs
instance Functor ExprF where
  fmap eval' (Const i) = Const i
  fmap eval' (left `Add` right) = (eval' left) `Add` (eval' right)
  fmap eval' (left `Mul` right) = (eval' left) `Mul` (eval' right)

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
type StringA = Algebra ExprF String

type ExprInitAlg = Algebra ExprF (Fix ExprF)

alg :: SimpleA
alg (Const i) = i
alg (x `Add` y) = x + y
alg (x `Mul` y) = x * y

pretty :: StringA
pretty (Const i) = show i
pretty (x `Add` y) = "(" ++ x ++ " + " ++ y ++ ")"
pretty (x `Mul` y) = "(" ++ x ++ " * " ++ y ++ ")"

testExpr' = (Const 3) `Mul` (Const 4)

ex_init_alg :: ExprF (Fix ExprF) -> Fix ExprF
ex_init_alg = In

-- k = fmap alg

unFix :: Fix f -> f (Fix f)
unFix (In x) = x

cata :: Functor f => (f a -> a) -> Fix f -> a
cata alg = alg. fmap (cata alg) . unFix

eval :: Fix ExprF -> Int
eval = alg . fmap eval . unFix

-- ListF algebra

data ListF a b = Nil | Cons a b deriving (Show)

instance Functor (ListF a) where
  fmap f Nil = Nil
  fmap f (Cons e x) = Cons e (f x)

algSum :: ListF Int Int -> Int
algSum Nil = 0
algSum (Cons e acc) = e + acc

lst :: Fix (ListF Int)
lst = In $ Cons 2 (In $ Cons 3 (In $ Cons 4 (In Nil)))


main = do
  print "123"
  print $ fmap alg testExpr'
  print $ eval $ testExpr
  print $ cata pretty $ testExpr
  print $ cata algSum lst
