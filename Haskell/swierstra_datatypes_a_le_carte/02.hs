{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE DeriveFunctor #-}

data Expr f = In (f (Expr f))

data Val e = Val Int
type IntExpr = Expr Val

expr1 = In $ Val 3

data Add e = Add e e
type AddExpr = Expr Add

infixr :+:
data (f :+: g) e = Inl (f e) | Inr (g e)

addExample :: Expr (Val :+: Add)
addExample = In (Inr (Add (In (Inl (Val 118))) (In (Inl (Val 1219)))))

instance Functor Val where
  fmap f (Val x) = Val x

instance Functor Add where
  fmap f (Add e1 e2) = Add (f e1) (f e2)

instance (Functor f, Functor g) => Functor (f :+:g) where
  fmap f (Inl e1) = Inl (fmap f e1)
  fmap f (Inr e2) = Inr (fmap f e2)

foldExpr :: Functor f => (f a -> a) -> Expr f -> a
foldExpr f (In t) = f (fmap (foldExpr f) t)

class Functor f => Eval f where
  evalAlgebra :: f Int -> Int

instance Eval Val where
  evalAlgebra (Val x) = x

instance Eval Add where  
  evalAlgebra (Add x y) = x + y

instance (Eval f, Eval g) => Eval (f :+: g) where
  evalAlgebra (Inl x) = evalAlgebra x
  evalAlgebra (Inr y) = evalAlgebra y

eval :: Eval f => Expr f -> Int
eval expr = foldExpr evalAlgebra expr

--val :: Int -> Expr Val
--val x = In (Val x) 

--infixl 6 .+.
--(.+.) :: Expr Add -> Expr Add-> Expr Add
--x .+. y = In (Add x y)


--val :: Int -> Expr f
--val x = In (Val x) 
--infixl 6 .+.
--(.+.) :: Expr f -> Expr f-> Expr f
--x .+. y = In (Add x y)

class (Functor sub, Functor sup) => sub :<: sup where 
  inj :: sub a -> sup a
  prj :: sup a -> Maybe (sub a)

instance Functor f => f :<: f where
  inj = id
  prj = Just

instance (Functor f, Functor g) => f :<: (f :+: g) where
  inj = Inl
  prj (Inl f) = Just f
  prj (Inr _) = Nothing

instance (Functor f, Functor g, Functor h, f :<: g) => f :<: (h :+: g) where
  inj = Inr . inj
  prj (Inr g) = prj g
  prj (Inl _) = Nothing


inject :: (g :<: f) => g (Expr f) -> Expr f
inject = In . inj

project :: (g :<: f) => Expr f -> Maybe (g (Expr f))
project (In t) = prj t

distr :: (Add :<: f, Mul :<: f) => Expr f -> Maybe (Expr f)
distr t = do
  Mul a b <- project t
  Add c d <- project b
  return (a.*.c .+. a.*.d)


val :: (Val :<: f) => Int -> Expr f
val x = inject (Val x)

infixl 6 .+.
(.+.) :: (Add :<: f) => Expr f -> Expr f -> Expr f
x .+. y = inject (Add x y)

x1 :: Expr (Add  :+: Val)
x1 = val 3000 .+. val 1330 .+. val 7

data Mul x = Mul x x
instance Functor Mul where
  fmap f (Mul x y) = Mul (f x) (f y)

instance Eval Mul where
  evalAlgebra (Mul x y) = x * y

infixl 7 .*.
(.*.) :: (Mul :<: f) => Expr f -> Expr f -> Expr f
x .*. y = inject (Mul x y)

-- doesn't compile
x2 :: Expr (Val :+: Add :+: Mul)
x2 = val 80 .*. val 5 .+. val 4

x3 :: Expr (Val :+: Mul)
x3 = val 6 .*. val 7

x4 :: Expr (Val :+: Add :+: Mul)
x4 = val 4 .*. (val 80 .+. val 5)


class Render f where
  render :: Render g => f (Expr g) -> String

pretty :: Render f => Expr f -> String
pretty (In t) = render t

instance Render Val where
  render (Val i) = show i

instance Render Add where
  render (Add x y) = "(" ++ pretty x ++ " + " ++ pretty y ++ ")"

instance Render Mul where
  render (Mul x y) = "(" ++ pretty x ++ " * " ++ pretty y ++ ")"

instance (Render f, Render g) => Render (f :+: g) where
  render (Inl x) = render x
  render (Inr y) = render y

distr_app = do
     x <- distr x4
     return $ pretty x




