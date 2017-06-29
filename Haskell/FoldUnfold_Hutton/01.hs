type Name = String
type Act = String


{-
data Proc = Con Name
          | Pre Act Proc
          | Cho [Proc]
          | Par Proc Proc
          | Res Proc Act
          | Rel Proc (Act -> Act)

data Tree = Node [(Act, Tree)]
-}

-- 5.1 Least fixpoints

newtype Fix f = In (f (Fix f))

type Proc = Fix P

data P p = Con Name
         | Pre Act p
         | Cho [p]
         | Par p p
         | Res p Act
         | Rel p (Act -> Act)


con n   = In (Con n)
pre a p = In (Pre a p)
cho ps  = In (Cho ps)
par p q = In (Par p q)
res p a = In (Res p a)
rel p f = In (Rel p f)

type Tree = Fix T

data T t = Node [(Act,t)]

-- 5.2 Functors

instance Functor P where
  fmap f x = case x of
    Con n   -> Con n
    Pre a p -> Pre a (f p)
    Cho ps  -> Cho [f p | p <- ps]
    Par p q -> Par (f p) (f q)
    Res p a -> Res (f p) a
    Rel p g -> Rel (f p) g

instance Functor T where
  fmap f (Node xs) = 
    Node [(a, f t) | (a,t) <- xs]

-- 6 Operational semantics of CCS
              
