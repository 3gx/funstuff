-- https://jtobin.io/sharing-in-haskell-edsls

{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TypeFamilies #-}

naiveTree :: (Eq a, Num a) => a -> a
naiveTree 0 = 1
naiveTree n = naiveTree (n-1) + naiveTree (n-1)

-- tree :: (Eq a, Num a) => a -> a
-- tree 0 = 1
-- tree n = let shared = tree (n-1)
--          in  shared + shared

data Expr = Lit Int
          | Add Expr Expr 
          | Let Expr (Expr -> Expr)
--    deriving (Eq, Ord)

instance Num Expr where
  fromInteger = Lit . fromInteger
  (+) = Add
  (Lit a) - (Lit b) = Lit (a-b)

eval :: Expr -> Int
eval (Lit d) = d
eval (Add e0 e1) = eval e0 + eval e1
eval (Let e0 e1) = let shared = Lit (eval e0)
                   in eval (e1 shared)

tree :: (Num a, Eq a) => a -> Expr
tree 0 = 1
tree n = Let (tree (n-1)) (\shared -> shared + shared)

-- > eval (tree 50)
-- 1125899906842624

