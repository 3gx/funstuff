{-# LANGUAGE DeriveFunctor #-}
data ExprF r = Const Int
             | Add r r
             | Mul r r
           deriving Functor

newtype Fix f = Fx (f (Fix f))
unFix :: Fix f -> f (Fix f)
unFix (Fx x) = x

cata :: Functor f => (f a -> a) -> Fix f -> a
cata alg = alg . fmap (cata alg) . unFix

alg :: ExprF Int -> Int
alg (Const i) = i
alg (x `Add` y) = x + y
alg (x `Mul` y) = x * y

pretty_alg :: ExprF String -> String
pretty_alg (Const i) = show i
pretty_alg (x `Add` y) = "(" ++ x ++ " + " ++ y ++ ")"
pretty_alg (x `Mul` y) = "(" ++ x ++ " * " ++ y ++ ")"

eval :: Fix ExprF -> Int
eval = cata alg

pretty_print :: Fix ExprF -> String
pretty_print = cata pretty_alg

-- show
testExpr = Fx $ (Fx $ (Fx $ Const 2) `Add` (Fx $ Const 3)) `Mul`
                (Fx $ Const 4)

main = do
  print $ eval $ testExpr
  print $ pretty_print $ testExpr
               
