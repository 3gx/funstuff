{-# OPTIONS_GHC -fno-warn-missing-methods #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE RankNTypes #-}

module PHOAS where

data Expr a =
    Lit Int
  | Var a
  | Add (Expr a) (Expr a)
  | Let (Expr a) (a -> Expr a)

instance Num (Expr a) where
  fromInteger = Lit . fromInteger
  e0 + e1     = Add e0 e1

eval :: Expr Int -> Int
eval (Lit d)     = d
eval (Var v)     = v
eval (Add e0 e1) = eval e0 + eval e1
eval (Let e f)   = eval (f (eval e))

tree :: (Num a, Eq a) => a -> Expr b
tree 0 = 1
tree n = Let (tree (n - 1)) ((\shared -> shared + shared) . Var)

text e = go e 0 where
  go (Lit j)     _ = show j
  go (Add e0 e1) c = "(Add " ++ go e0 c ++ " " ++ go e1 c ++ ")"
  go (Var x) _     = x
  go (Let e0 e1) c = "(Let " ++ v ++ " " ++ go e0 (c + 1) ++
                     " in " ++ go (e1 v) (c + 1) ++ ")"
    where v = "v" ++ show c

