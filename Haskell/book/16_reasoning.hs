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

{-
  exec (comp e) s = eval e : s

 Base case:
 ----------

   exec (comp (Val n)) s  -- apply comp

== exec ([PUSH n]) s   -- apply exec
 
== exec [] (n : s) = n : s  -- unappy eval

== eval (Val n)) : s


 Inductive case:
 --------------
 
   exec (comp (Add x y)) s  -- apply comp

== exec (comp x ++ comp y ++ [ADD]) s  -- associativeity of ++

== exec (comp x ++ (comp y ++ [ADD])) s  -- distributivity

== exec (comp y ++ [ADD]) (exec (comp x) s)  -- induction hypothesis for x

== exec (comp y ++ [ADD}) (eval x : s) -- distributivity again

== exec [ADD] (exec (comp y) (eval x : s)) -- induction hypothesis for y

== exec [ADD] (eval y : eval x : s) -- apply exec

== (eval x + eval y ) : s -- unapply eval

== eval (Add x y) : s


  Distributivity:
  --------------
  
  exec (c ++ d) s = exec d (exec c s)

-}

comp' :: Expr -> Code -> Code
comp' (Val n) c = PUSH n : c
comp' (Add x y) c = comp' x (comp' y (ADD : c))

-- > comp' e []
-- [PUSH 2,PUSH 3,ADD,PUSH 4,ADD]
-- *Main> exec (comp' e [])  []
-- [9]


