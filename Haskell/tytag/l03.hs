-- Chapter 3 from lecture.pdf in  
--   http://okmij.org/ftp/tagless-final/course/index.html

{-# LANGUAGE NoMonomorphismRestriction #-}

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

  lam e     = S $ \h -> let x = "f" ++ show h
                        in "(\\" ++ x ++ " -> " ++ 
                            unS (e (S $ const x)) (succ h) ++ ")"
  app e1 e2 = S $ \h -> "(" ++ unS e1 h ++ " " ++ unS e2 h ++ ")"


eval :: R a -> a
eval e = unR e

view e = unS e 0

main = do
  print $ view th1
  print $ eval th1
  print $ view th2
  print $ eval th2 $ 3
  print $ view th3
  print $ eval th3 $ (*3)
  print $ view th4
  print $ eval th4  (*3) (+7)
