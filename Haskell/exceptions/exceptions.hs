{-# LANGUAGE GADTs #-}
-- Syntax

data Expr = Val Int 
          | Add Expr Expr 
          | Throw
          | Catch Expr Expr
       deriving (Show)

-- Semantics:

eval :: Expr -> Int
eval (Val n) = n
eval (Add x y) = eval x + eval y

-- Expression

e = Add (Val 3) (Val 5)

-- > e
-- Add (Val 3) (Val 5)
-- > eval e
-- 8

----- ~~~~~~~~~~~~~~~ -----
----- ~~~~~~~~~~~~~~~ -----

-- Step 1 - Add Continuations
------------------------------

-- Make the evaluation order _explicit_, by rewriting the semantics in 
-- _continuation-passing_ style (CPS)
--
-- Def: A continuation is a functino that is applied to the result of 
--      another computation

-- example:  (eval x)       (+ eval y)
--           |_ computtion  |___ continuation
--
-- Basic idea: Generalize semantics to make the use of continuations explicit

{-
  Define new semantics:
      eval' :: Expr -> (Int -> Int) -> Int
                           |___ continuation applies to result of calculation
  such that
      eval' e c = c (eval e)
  and hence 
      eval e = eval' e (\n -> n)
-}

type Cont = Int -> Int

-- evaluation order is explicit
--    first eval' x
--     then eval' y
--      thena apply (+)
eval' :: Expr -> Cont -> Int
eval' (Val n) c = c n
eval' (Add x y) c = eval' x (\n ->
                               eval' y (\m -> c (n+m)))

-- > eval' e (+100)
-- 108
-- > eval' e id
-- 8


----- ~~~~~~~~~~~~~~~ -----
----- ~~~~~~~~~~~~~~~ -----


-- Step 2 - Defunctionalization
--------------------------------

-- Abstract machines are first-order concepts. 
-- We need to simulate higher orderness

-- Basic idea: Represent the continuations we actually need using a datatype

eval0 :: Expr -> Int
eval0 e = eval' e id

-- > eval0 e
-- 8

-- Combinators  , time: 22:52

c1 :: Cont
c1 = id  -- \n -> n

c2 :: Expr -> Cont -> Cont
c2 y c = \n -> eval' y (c3 n c)

c3 :: Int -> Cont -> Cont
c3 n c = \m -> c (n + m)

eval1' :: Expr -> Cont -> Int
eval1' (Val n) c = c n
eval1' (Add x y) c = eval1' x (c2 y c)

eval1 :: Expr -> Int
eval1 e = eval1' e c1


-- Now apply defunctionalization

-- CONT: abstract machine representation of continuation
data CONT = C1
          | C2 Expr CONT
          | C3 Int  CONT

apply :: CONT -> Cont
apply C1 = c1
apply (C2 y c)  = c2 y (apply c)
apply (C3 n c) = c3 n (apply c)

data CONT' where
  C1' :: CONT'
  C2' :: Expr -> CONT' -> CONT'
  C3' :: Int -> CONT' -> CONT'

-- Semantics are now 1st order (CONT is not a function type, it is
-- representation of continuation)
-----------------------------------------------------------------------

eval'' :: Expr -> CONT -> Int
--eval'' e c = eval' e (apply c)

eval'' (Val n) c = apply c n -- can be apply'' c n
eval'' (Add x y) c = eval'' x (C2 y c)

apply'' :: CONT -> Int -> Int
apply'' C1 n = n
apply'' (C2 y c) n = eval'' y (C3 n c)
apply'' (C3 n c) m = apply c (n+m) 


eval2 :: Expr -> Int
eval2 e = eval'' e C1

----- ~~~~~~~~~~~~~~~ -----
----- ~~~~~~~~~~~~~~~ -----


-- Step 3 : Refactor
---------------------

-- Abstract machine

-- Control stack type
data Contr = STOP
           | EVAL Expr Contr
           | ADD  Int  Contr

run :: Expr -> Int
run e = evalr e STOP

-- used to be called eval''
evalr :: Expr -> Contr -> Int
evalr (Val n) c = exec c n
evalr (Add x y) c = evalr x (EVAL y c)

-- used to be called apply
exec :: Contr -> Int -> Int
exec STOP n = n
exec (EVAL y c) n = evalr y (ADD n c)
exec (ADD  n c) m = exec c (n+m)

{-
   run (Add (Val 1) (Val 2)) 
 = evalr (Add (Val 1) (Val 2)) STOP
 = evalr (Val 1) (EVAL (Val 2) STOP)
 = exec (EVAL (Val 2) STOP) 1
 = evalr (Val 2) (ADD 1 STOP)
 = exec (ADD 1 STOP) 2
 = exec 3 STOP
 = 3
-}

-- > run e
-- 8


----- ~~~~~~~~~~~~~~~ -----
----- ~~~~~~~~~~~~~~~ -----

-- ~~~=== Adding Exceptions ===~~~


