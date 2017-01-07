-- http://kseo.github.io//posts/2017-01-03-writing-an-interpreter-using-fold.html
--

data Expr = Const Int
          | Add Expr Expr
          | Mul Expr Expr deriving (Show)

interp :: Expr -> Int
interp (Const x) = x
interp (Add e1 e2) = interp e1 + interp e2
interp (Mul e1 e2) = interp e1 * interp e2

pretty :: Expr -> String
pretty (Const x) = show x
pretty (Add e1 e2) = "(" ++ pretty e1 ++ " + " ++ pretty e2 ++ ")"
pretty (Mul e1 e2) = "(" ++ pretty e1 ++ " * " ++ pretty e2 ++ ")"

data ExprA a = ExprA {
  val::Int->a, add::a->a->a, mul::a->a->a
}

foldExpr :: ExprA a -> Expr -> a
foldExpr alg (Const i) = val alg i
foldExpr alg (Add e1 e2) = add alg (foldExpr alg e1) (foldExpr alg e2)
foldExpr alg (Mul e1 e2) = mul alg (foldExpr alg e1) (foldExpr alg e2)

interpA :: ExprA Int
interpA = ExprA {
  val = id, add = (+), mul = (*)
}

prettyA :: ExprA String
prettyA = ExprA {
  val = show,  
  add = \a b->"(" ++a++ " + " ++b++ ")",
  mul = \a b->"(" ++a++ " * " ++b++ ")"
}

interp' :: Expr -> Int
interp' = foldExpr interpA

pretty' :: Expr -> String
pretty' = foldExpr prettyA

g = Mul (Const 2) $ Add (Const 3) (Const 5)
main = do
  print "123"
  print g
  print $ interp g
  print $ pretty g
  print $ interp' g
  print $ pretty' g

