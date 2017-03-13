-- Chapter 3 from lecture.pdf in  
--   http://okmij.org/ftp/tagless-final/course/index.html

{-# LANGUAGE GADTs #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
-- {-# LANGUAGE StandaloneDeriving #-}

-- 3.4 Tagless final embedding with higher-order abstract syntax
----------------------------------------------------------------

class Semantics repr where
  int :: Int -> repr Int
  add :: repr Int -> repr Int -> repr Int

  lam :: (repr a -> repr b) -> repr (a->b)
  app :: repr (a->b) -> repr a -> repr b

-- th1 :: (Semantics repr) => repr Int
th1 = add (int 1) (int 2)

-- th2 :: (Semantics repr) => repr (Int -> Int)
th2 = lam (\x -> add x x)

-- th3 :: (Semantics repr) => repr ((Int -> Int) -> Int)
th3 = lam (\x -> add (app x (int 1)) (int 2))
th4 = lam (\x -> lam(\y -> add (app x (int 1)) (app y (int 2))))

newtype R a = R { unR :: a }

instance Semantics R where
  int x     = R x
  add e1 e2 = R $ unR e1 + unR e2
  
  lam f     = R $ unR . f . R
  app e1 e2 = R $ (unR e1) (unR e2)

type VarCounter = Int
newtype S a = S { unS :: VarCounter -> String }

instance Semantics S where
  int x     = S $ const $ show x
  add e1 e2 = S $ \h -> "(" ++ unS e1 h ++ "+" ++ unS e2 h ++ ")"

  lam e     = S $ \h -> let x = "x" ++ show h
                        in "(\\" ++ x ++ " -> " ++ 
                            unS (e (S $ const x)) (succ h) ++ ")"
  app e1 e2 = S $ \h -> "(" ++ unS e1 h ++ " " ++ unS e2 h ++ ")"


eval :: R a -> a
eval e = unR e


view :: S a -> String
view e = unS e 0 

-- Extensibility

class MulSYM repr where
  mul :: repr Int -> repr Int -> repr Int

class BoolSYM repr where
  bool :: Bool -> repr Bool
  leq  :: repr Int -> repr Int -> repr Bool
  if_  :: repr Bool -> repr a -> repr a -> repr a

class FixSYM repr where
  fix :: (repr a -> repr a) -> repr a

-- The inferred type of pow shows all the extensions in action

tpow  = lam (\x -> fix (\self -> lam (\n ->
                        if_ (leq n (int 0)) (int 1)
                            (mul x (app self (add n (int (-1))))))))
-- > :t tpow
-- tpow
--   :: (Semantics repr, FixSYM repr, BoolSYM repr, MulSYM repr) =>
--      repr (Int -> Int -> Int)

tpow7 = lam (\x -> app (app tpow x) (int 7))
tpow72 = app tpow7 (int 2)
-- > :t tpow7
-- tpow7
--   :: (Semantics repr, MulSYM repr, BoolSYM repr, FixSYM repr) =>
--      repr (Int -> Int)
-- > :t tpow72
-- tpow72
--   :: (Semantics repr, FixSYM repr, BoolSYM repr, MulSYM repr) =>
--      repr Int

instance MulSYM R where
  mul e1 e2 = R $ unR e1 * unR e2

instance BoolSYM R where
  bool b       = R b
  leq e1 e2    = R $ unR e1 <= unR e2
  if_ be et ee = R $ if unR be then unR et else unR ee

instance FixSYM R where
  fix f = R $ fx (unR . f . R) where fx f = f (fx f)

instance MulSYM S where
    mul e1 e2 = S $ \h -> 
      "(" ++ unS e1 h ++ "*" ++ unS e2 h ++ ")"

instance BoolSYM S where
    bool x     = S $ const $ show x
    leq e1 e2 = S $ \h -> 
      "(" ++ unS e1 h ++ "<=" ++ unS e2 h ++ ")"
    if_ be et ee = S $ \h ->
       unwords["(if", unS be h, "then", unS et h, "else", unS ee h,")"]

instance FixSYM S where
    fix e = S $ \h -> 
       let self = "self" ++ show h
       in "(fix " ++ self ++ "." ++ 
            unS (e (S $ const self)) (succ h) ++ ")"

-- 3.5 Relating initial and final typed tagless encodings 
---------------------------------------------------------

-- requires GADTs
data IR h t where
  INT  :: Int  -> IR h Int
  BOOL :: Bool -> IR h Bool

  Add :: IR h Int -> IR h Int -> IR h Int
  Mul :: IR h Int -> IR h Int -> IR h Int
  Leq :: IR h Int -> IR h Int -> IR h Bool
  IF  :: IR h Bool -> IR h t -> IR h t -> IR h t

  Var :: h t -> IR h t
  Lam :: (IR h t1 -> IR h t2) -> IR h (t1 -> t2)
  App :: IR h (t1 -> t2) -> IR h t1 -> IR h t2
  Fix :: (IR h t -> IR h t) -> IR h t


ti1 = Add (INT 1) (INT 2)
ti2 = Lam(\x -> Add x x)
ti3 = Lam(\x -> Add (App x (INT 1)) (INT 2)) 

tipow  = Lam (\x -> Fix (\self -> Lam (\n ->
                        IF (Leq n (INT 0)) (INT 1)
                            (Mul x (App self (Add n (INT (-1))))))))

tipow7 = Lam (\x -> App (App tipow x) (INT 7))
tipow72 = App tipow7 (INT 2)

evalI :: IR R t -> t
evalI (INT n)   = n
evalI (BOOL n)  = n
evalI (Add e1 e2) = evalI e1 + evalI e2
evalI (Mul e1 e2) = evalI e1 * evalI e2
evalI (Leq e1 e2) = evalI e1 <= evalI e2
evalI (IF be et ee) = if (evalI be) then evalI et else evalI ee
evalI (Var v) = unR v
evalI (Lam b) = \x -> evalI (b . Var . R $ x)
evalI (App e1 e2) = (evalI e1) (evalI e2)
evalI (Fix f) = evalI (f (Fix f))

viewI' :: IR S t -> VarCounter -> String
viewI' (INT x)  = const $ show x
viewI' (BOOL x) = const $ show x
viewI' (Add e1 e2) = \h -> 
      "(" ++ viewI' e1 h ++ "+" ++ viewI' e2 h ++ ")"
viewI' (Mul e1 e2) = \h -> 
      "(" ++ viewI' e1 h ++ "*" ++ viewI' e2 h ++ ")"
viewI' (Leq e1 e2) = \h -> 
      "(" ++ viewI' e1 h ++ "<=" ++ viewI' e2 h ++ ")"
viewI' (IF be et ee) = \h -> 
       unwords["(if", viewI' be h, "then", viewI' et h, "else", viewI' ee h,")"]

viewI' (Var x) = unS x
viewI' (Lam e) = \h ->
       let x = "x" ++ show h
       in "(\\" ++ x ++ " -> " ++ 
            viewI' (e (Var . S $ const x)) (succ h) ++ ")"
viewI' (App e1 e2) = \h -> 
      "(" ++ viewI' e1 h ++ " " ++ viewI' e2 h ++ ")"
viewI' (Fix e) = \h ->
       let self = "self" ++ show h
       in "(fix " ++ self ++ "." ++ 
            viewI' (e (Var . S $ const self)) (succ h) ++ ")"

viewI e = viewI' e 0

instance Semantics (IR h) where
    int  = INT
    add  = Add

    lam  = Lam
    app  = App

instance MulSYM (IR h) where
    mul  = Mul

instance BoolSYM (IR h) where
    bool = BOOL
    leq  = Leq
    if_  = IF

instance FixSYM (IR h) where
    fix  = Fix

f2i :: IR h t -> IR h t
f2i = id 

-- * From Initial to Final

i2f :: (Semantics repr, BoolSYM repr, MulSYM repr, FixSYM repr) => 
       IR repr t -> repr t
i2f (INT x)  = int x
i2f (BOOL x) = bool x
i2f (Add e1 e2) = add (i2f e1) (i2f e2)
i2f (Mul e1 e2) = mul (i2f e1) (i2f e2)
i2f (Leq e1 e2) = leq (i2f e1) (i2f e2)
i2f (IF be et ee) = if_ (i2f be) (i2f et) (i2f ee)
i2f (Var v) = v -- polymorphic lift
i2f (Lam e) = lam(\x -> i2f (e (Var x)))
i2f (App e1 e2) = app (i2f e1) (i2f e2)
i2f (Fix e) = fix(\x -> i2f (e (Var x)))


main = do
  print $ view th1
  print $ eval th1
  print $ view th2
  print $ eval th2 $ 3
  print $ view th3
  print $ eval th3 $ (*3)
  print $ view th4
  print $ eval th4  (*3) (+7)
  print $ view tpow
  print $ eval tpow 2 8
  print $ view tpow7
  print $ eval tpow7 2 
  print $ view tpow72
  print $ eval tpow72 
