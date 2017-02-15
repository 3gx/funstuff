{-# OPTIONS_GHC -fno-warn-missing-methods #-}

module HOAS where

data Expr =
    Lit Int
  | Add Expr Expr
  | Let Expr (Expr -> Expr)

instance Num Expr where
  fromInteger = Lit . fromInteger
  (+)         = Add

eval :: Expr -> Int
eval (Lit d)     = d
eval (Add e0 e1) = eval e0 + eval e1
eval (Let e0 e1) =
  let shared = Lit (eval e0)
  in  eval (e1 shared)

tree :: (Num a, Eq a) => a -> Expr
tree 0 = 1
tree n = Let (tree (n - 1)) (\shared -> shared + shared)
