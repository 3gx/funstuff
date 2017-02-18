-- 17 Calculating compilers

-- 17.2 Syntax & semantics

data Expr = Val Int | Add Expr Expr

eval :: Expr -> Int
eval (Val n) = n
eval (Add x y) = eval x + eval y

e = Add (Add (Val 2) (Val 3)) (Val 4)

-- > eval e
-- 9


-- 17.3 Adding a stack

type Stack = [Int]

eval' :: Expr -> Stack -> Stack
eval' (Val n) s = push n s
eval' (Add x y) s = add (eval' y (eval' x s))

push :: Int -> Stack -> Stack
push n s = n : s

add :: Stack -> Stack
add (m:n:s) = n+m : s

eval1 :: Expr -> Int
eval1 e = head (eval' e [])

-- 17.4 Adding a continuation

-- Transform the stack-based evaluation function eval' into continuation-passing 
-- style (CPS) in order to make the flow of control explicit

type Cont = Stack -> Stack

eval'' :: Expr -> Cont -> Cont

-- eval'' e c s = c (eval' e s)

{- 
   Base case:
   ----------

    eval'' (Val n) c s = 

==  c (eval' (Val n) s) 

==  c (push n s)

  Inductive case:
  ---------------

    eval'' (Add x y) c s

== c (eval' (add x y) s)

== c (add (eval' y (eval' x s)))

== (c.add) (eval' y (eval' x s))

== eval'' y (c.add) (eval' x s)

== eval'' y (eval'' x (c.add)) s

-}

eval'' (Val n) c s = c (push n s)
eval'' (Add x y) c s = eval'' x (eval'' y (c.add)) s

eval1' :: Expr -> Cont
eval1' e s = eval'' e id s


-- > eval1' e []
-- [9]

-- 17.5 Defunctionalizing


haltC :: Cont
haltC = id

pushC :: Int -> Cont -> Cont
pushC n c = c . push n

addC :: Cont -> Cont
addC c = c . add

eval2' :: Expr -> Cont
eval2' e = eval2'' e haltC

eval2'' :: Expr -> Cont -> Cont
eval2'' (Val n) c = pushC n c
eval2'' (Add x y) c = eval2'' x (eval2'' y (addC c))

data Code = HALT | PUSH Int Code | ADD Code deriving Show

exec0 :: Code -> Cont
exec0 HALT = haltC
exec0 (PUSH n c) = pushC n (exec0 c)
exec0 (ADD c) = addC (exec0 c)

exec :: Code -> Stack -> Stack
exec HALT s = s
exec (PUSH n c) s = exec c (n:s)
exec (ADD c) (m:n:s) = exec c (n+m:s)

comp :: Expr -> Code
comp e = comp' e HALT

comp' :: Expr -> Code -> Code
comp' (Val n) c = PUSH n c
comp' (Add x y) c = comp' x (comp' y (ADD c))

-- > exec (comp e) []
-- [9]
-- > comp e
-- PUSH 2 (PUSH 3 (ADD (PUSH 4 (ADD HALT))))

-- 17.7 Combining the steps
--
--  1. calculate a generaized evaluation function that uses a stack
--  2. calculate a further generalized version that using a continuation
--  3. defunctionalize to produce a compiler and a virtual machine

{-
  Start by defining 1) a type Expr to represent syntax of source language,
  together with 2) an evaluation function eval :: Expr -> Int that provides
  semantics for the language, and 3) a type Stack that represents a stack of 
  integer values.

  Then derive four additional components:

    * a type Code that represents code for the virtual machine
    * a function comp :: Expr -> Code that compiles expressions to code
    * a function comp' :: Expr -> Code -> Code with a code argument
    * a function exec :: Code -> Stack -> Stack taht executes code

    relationship between sematncsi, compiler & virtual machinees are captured
    following two correctness equations:
 
    exec (comp e) s = eval e : s
    exec (comp' e c) s = exec c (eval e : s)

   To combine the transformation step is to use these two eq. directly as a 
   _specification_ for the four additional components. 
-}
  
  

