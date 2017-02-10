data Expr = Val Int | Div Expr Expr

eval :: Expr -> Int
eval (Val n) = n
eval (Div x y) = eval x `div` eval y

safediv :: Int -> Int -> Maybe Int
safediv _ 0 = Nothing
safediv m n = Just (m `div` n)

eval' :: Expr -> Maybe Int
eval' (Val n) = Just n
eval' (Div x y) = eval' x >>= \n -> eval' y >>= \m ->  safediv n m

-- > eval' (Div (Val 6) (Val 0))
-- Nothing
-- > eval' (Div (Val 6) (Val 3))
-- Just 2

data Tree a = Leaf a | Node (Tree a) (Tree a) deriving Show

tree :: Tree Char
tree = Node (Node (Leaf 'a') (Leaf 'b')) (Leaf 'c')

rlabel :: Tree a -> Int -> (Tree Int, Int)
rlabel (Leaf _) n = (Leaf n, n+1)
rlabel (Node l r) n = (Node l' r' , n'')
  where
    (l', n') = rlabel l n
    (r', n'') = rlabel r n'

-- > fst (rlabel tree 0)
-- Node (Node (Leaf 0) (Leaf 1)) (Leaf 2)


type State = Int
newtype ST a = S (State -> (a, State))
app :: ST a -> State -> (a,State)
app (S st) x = st x

instance Functor ST where
  fmap g st = S (\s -> let (x,s') = app st s in (g x, s'))

instance Applicative ST where
  pure x = S (\s -> (x,s))
  stf <*> stx = S (\s -> let (f,s') = app stf s
                             (x,s'') = app stx s'
                        in (f x, s''))

instance Monad ST where 
  return = pure
  stx >>= f = S (\s -> let (x,s') = app stx s in app (f x) s')

fresh :: ST Int
fresh = S (\n -> (n,n+1))

alabel :: Tree a -> ST (Tree Int)
alabel (Leaf _) = Leaf <$> fresh
alabel (Node l r) = Node <$> alabel l <*> alabel r

-- > fst $ app (alabel tree) 0
-- Node (Node (Leaf 0) (Leaf 1)) (Leaf 2)

mlabel :: Tree a -> ST (Tree Int)
mlabel (Leaf _) = do n <- fresh
                     return (Leaf n)

mlabel (Node l r) = do l' <- mlabel l
                       r' <- mlabel r
                       return (Node l' r')
                  
-- > fst $ app (mlabel tree) 0
-- Node (Node (Leaf 0) (Leaf 1)) (Leaf 2)


