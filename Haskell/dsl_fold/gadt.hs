{-# LANGUAGE GADTs #-}

------------------------------------------
------------------------------------------

{-
data Expr = I Int
          | Add Expr Expr
          | Mul Expr Expr 
       deriving (Show)

expr = (I 5 `Add` I 1) `Mul` (I 7) :: Expr

eval :: Expr -> Int
eval (I n) = n
eval (Add e1 e2) = eval e1 + eval e2
eval (Mul e1 e2) = eval e1 * eval e2
-}

------------------------------------------
------------------------------------------

{-
data Expr = I Int
          | B Bool
          | Add Expr Expr
          | Mul Expr Expr 
          | Eq Expr Expr
       deriving (Show)

expr = (I 5 `Add` I 1) `Eq` I 7 :: Expr

{-
eval :: Expr -> §Either Int Bool
eval (I n) = Left n
eval (B b) = Right b
--              Doesn't typecheck
--              :t eval e1 :: Either Int Bool
eval (Add e1 e2) = eval e1 + eval e2
-}
eval :: Expr -> Maybe (Either Int Bool)
eval (I n) = Just (Left n)
eval (B b) = Just (Right b)
--eval (Add e1 e2) = eval e1 + eval e2
-}

------------------------------------------
------------------------------------------

-- Want more type safety!

{-
-- Phantom types
-- a - is a phantom type
--      unlike [a], there is no value of type a inside Expr a
--      it is a marker/dummy type, called phantom type
data Expr a = I Int
            | B Bool
            | Add (Expr a) (Expr a)
            | Mul (Expr a) (Expr a)
            | Eq  (Expr a) (Expr a)
  deriving Show

--Add :: Expr a -> Expr a -> Expr a
--          Int is a restricted type
add :: Expr Int -> Expr Int -> Expr Int
add = Add

b :: Bool -> Expr Bool
b = B

i :: Int -> Expr Int
i = I

-- doesn't typecheck!
{-
eq :: Expr Int -> Expr Int -> Expr Bool
eq = Eq
-}

-- add & eq are smart constructors
--
-- b True `add` i 5 won't type-check!

{-  -- fails typecheck
eval :: Expr a -> a
eval (I n) = n
-}

-}

------------------------------------------
------------------------------------------


-- GADTs!

-- constructors are restricted
--  I is retricted on Int
--  B is restricted on Bool
data Expr a where
  I :: Int -> Expr Int
  B :: Bool -> Expr Bool
  Add :: Expr Int -> Expr Int -> Expr Int
  Mul :: Expr Int -> Expr Int -> Expr Int
  Eq  :: Expr Int -> Expr Int -> Expr Bool

eval :: Expr a -> a
eval (I n) = n
eval (B b) = b
eval (Add e1 e2) = eval e1 + eval e2
eval (Mul e1 e2) = eval e1 * eval e2
eval (Eq e1 e2)  = eval e1 == eval e2

expr = (I 5 `Add` I 1) `Mul` (I 7) 
rhs = I 42
rhs1 = I 43

main = do 
  print $ eval (expr `Eq` rhs)
  print $ eval (expr `Eq` rhs1)




