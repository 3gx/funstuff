-- 16.7 Compiler correctness

data Expr = Val Int | Add Expr Expr  deriving Show

eval :: Expr -> Int
eval (Val n) = n
eval (Add x y) = eval x + eval y

type Stack = [Int]
type Code = [Op]

data Op = PUSH Int | ADD  deriving Show

exec :: Code -> Stack -> Stack
exec [] s = s
exec (PUSH n : c) s = exec c (n:s)
exec (ADD : c) (m:n:s) = exec c (n+m : s)

comp :: Expr -> Code
comp (Val n) = [PUSH n]
comp (Add x y) = comp x ++ comp y ++ [ADD]

e = Add (Add (Val 2) (Val 3)) (Val 4)

-- > e
-- Add (Add (Val 2) (Val 3)) (Val 4)
-- > eval e
-- 9
-- > comp e
-- [PUSH 2,PUSH 3,ADD,PUSH 4,ADD]
-- > exec (comp e) []
-- [9]



