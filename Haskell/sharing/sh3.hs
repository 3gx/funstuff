
data Expr a = Lit Int
            | Add (Expr a) (Expr a)
            | Let (Expr a) (a -> Expr a)
            | Var a

eval :: Expr Int -> Int
eval (Lit d) = d
eval (Var v) = v
eval (Add e0 e1) = eval e0 + eval e1
eval (Let e f) = eval (f (eval e)) 

instance (Num a, Eq a) => Num (Expr a) where
  fromInteger = Lit . fromInteger
  (+) = Add
  (Lit a) - (Lit b) = Lit (a-b)

tree :: (Num a, Eq a) => a -> Expr a
tree 0 = 1 
tree n = Let (tree (n-1)) ((\shared -> shared + shared) . Var)

text :: Expr Int -> String
text e = go e 0 where
  go (Lit j)     _ = show j
  go (Add e0 e1) c = "(Add " ++ go e0 c ++ " " ++ go e1 c ++ ")"
  go (Var x) _     = show x
  go (Let e0 e1) c = "(Let " ++ v ++ " " ++ go e0 (c + 1) ++
                    " in " ++ go (e1 (eval e0)) (c + 1) ++ ")"
    where v = "v" ++ show c

-- > text $ tree 4
-- "(Let v0 (Let v1 (Let v2 (Let v3 1 in (Add 1 1)) in (Add 2 2)) in (Add 4 4))
-- in (Add 8 8))"
-- > eval $ tree 4
-- 16
-- > eval $ tree 50
-- 1125899906842624
--

