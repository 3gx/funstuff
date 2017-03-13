-- Chapter 3 from lecture.pdf in  
--   http://okmij.org/ftp/tagless-final/course/index.html
--

{-# LANGUAGE GADTs #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

-- 3.1 The problem of tags
--------------------------

data Exp0 = V0 Var0
         | B0 Bool
         | L0 Exp0
         | A0 Exp0 Exp0
  deriving Show

data Var0 = VZ0 | VS0 Var0
  deriving Show

ti1' = A0 (L0 (V0 VZ0)) (B0 True)

lookp0 VZ0 (x:_) = x
lookp0 (VS0 v) (_:env) = lookp0 v env

data U = UB Bool | UA (U -> U)

instance Show U where
  show (UB x) = "UB " ++ show x
  show (UA _) = "UA <fun>"

eval0 :: [U] -> Exp0 -> U
eval0 env (V0 v) = lookp0 v env
eval0 env (B0 b) = UB b
eval0 env (L0 e) = UA (\x -> eval0 (x:env) e)
eval0 env (A0 e1 e2) = case eval0 env e1 of
                      UA f -> f (eval0 env e2)

ti1'_eval = eval0 [] ti1'

{-
   eval0 [] A0 (L0 (V0 VZ0)) (B0 True)
       |  env = []
       |  e1  = (L0 (V0 VZ0))
       |  e2  = (B0 True)

 = case eval0 [] (L0 (V0 VZ0)) of
    |   UA f -> f (eval0 [] (B0 True))
    | 
    |  eval0 [] (L0 (V0 VZ0))
    |     | env = []
    |     | e   = (V0 VZ0)
    |     = UA (\x -> eval0 (x:[]) (V0 VZ0))
    |  f = \x -> eval0 (x:[]) (V0 VZ0)
    |  f (eval0 [] (B0 True))
           |   env = []
           |   b   = True
           = UB True
    |  f (UB True)
= eval0 (UB True:[]) (V0 VZ0)
|    env = (UB True:[])
|    v   = VZ0
= loopkp VZ0 (UB True:[])
= UB True

   eval0 [] A0 (L0 (B0 False)) (B0 True)
 | env = []
 | e1  = (L0 (B0 False))
 | e2  = (B0 True)
 = case eval0 [] (L0 (B0 False)) of
         UA f -> f (eval0 [] (B0 True))
 |   eval0 [] (L0 (B0 False))
 |   | env = []
 |   | e   = B0 False
 |   UA \x -> eval0 (x:[]) B0 False
 eval0 (B0 True:[]) B0 False
 | env = B0 True:[]
 | b = False
 UB False
 -}

ti2a = A0 (B0 True) (B0 False)
ti2a_eval = eval0 [] ti2a

ti2o = A0 (L0 (V0 (VS0 VZ0))) (B0 True) 
ti2o_eval = eval0 [] ti2o

{-
typecheck :: Exp0 -> Either ErrMsg Exp0
type ErrMsg  String
-}

-- 3.2 Tagless, initial & final embeddings
------------------------------------------

data Exp env t where 
  B :: Bool           -> Exp env Bool
  V :: Var env t      -> Exp env t
  L :: Exp (a,env) b  -> Exp env (a->b)
  A :: Exp env (a->b) -> Exp env a -> Exp env b

data Var env t where
  VZ :: Var (t,env) t
  VS :: Var env t -> Var (a,env) t


ti1 = A (L (V VZ)) (B True)

lookp :: Var env t -> env -> t
lookp VZ (x,_) = x
lookp (VS v) (_,env) = lookp v env


eval1 :: env -> Exp env t -> t
eval1 env (V v) = lookp v env
eval1 env (B b) = b
eval1 env (L e) = \x -> eval1 (x,env) e
eval1 env (A e1 e2) = (eval1 env e1) (eval1 env e2)

ti1_eval = eval1 () ti1

ti2oo = A (L (V (VS VZ))) (B True) 

vz0 (vc,_) = vc
vs0 vp (_, envr) = vp envr

b0 bv env    = bv
l0 e  env    = \x -> e (x,env)
a0 e1 e2 env = (e1 env) (e2 env)

tf1' = a0 (l0 vz0) (b0 True)
tf1'_eval = tf1' ()

-- 3.3 Tagless final embedding with de Bruijn indices
------------------------------------------------------

class Semantics repr where
  int :: Int -> repr h Int
  add :: repr h Int -> repr h Int -> repr h Int

  z   :: repr (a,h) a
  s   :: repr h a -> repr (any,h) a
  lam :: repr (a,h) b -> repr h (a->b)
  app :: repr h (a->b) -> repr h a -> repr h b

-- td1 :: (Semantics repr) => repr h Int
td1 = add (int 1) (int 2)

-- td2o ::: (Semantics repr) => repr (Int, h) (Int -> Int)
td2o = lam (add z (s z))

-- td3 :: (Semantics repr) => rerp h  ((Int -> Int) -> Int)
td3 = lam (add (app z (int 1)) (int 2))

newtype R h a = R { unR:: h -> a}

instance Semantics R where
  int x     = R $ const x
  add e1 e2 = R $ \h -> (unR e1 h) + (unR e2 h)

  z         = R $ \(x,_) -> x
  s v       = R $ \(_,h) -> unR v h
  lam e     = R $ \h -> \x -> unR e (x,h)
  app e1 e2 = R $ \h -> (unR e1 h) (unR e2 h)

eval e = unR e ()

newtype S h a = S { unS :: Int -> String }

instance Semantics S where
  int x     = S $ const $ show x
  add e1 e2 = S $ \h -> "(" ++ unS e1 h ++  " + " ++ unS e2 h ++ ")"

  z         = S $ \h -> "x" ++ show (h-1)
  s v       = S $ \h -> unS v (h-1)
  lam e     = S $ \h -> let x = "x" ++ show h
                        in "(\\" ++ x ++ " -> " ++ unS e(h+1) ++ ")"
  app e1 e2 = S $ \h -> "(" ++ unS e1 h  ++ " " ++ unS e2 h ++ ")"

view :: S () a -> String
view e = unS e 0

main = do
  print $ ti1'
  print $ ti1'_eval
  print $ ti1_eval
  print $ tf1'_eval
  print $ view td1
  print $ eval td1
  print $ view td3
  print $ eval td3 $ (*2)
  print $ unS td2o 1
  print $ unR td2o (3,()) $ 5

