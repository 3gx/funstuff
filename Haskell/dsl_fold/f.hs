{-# LANGUAGE UndecidableInstances, RankNTypes #-}
data ExprF a = Const Int
             | Add a a
             | Mul a a  

data Fix f = In ( f (Fix f))

-- requires UndecidableInstances
instance Show (f (Fix f)) => Show (Fix f) where
  show (In i) = "(In $ " ++ show i ++ ")"

instance Show a => Show (ExprF a)where
  show (Const i) = "(Const " ++ show i ++")"
  show (Add x y) = "(Add " ++ show x ++ show y ++ ")"
  show (Mul x y) = "(Mul " ++ show x ++ show y ++ ")"

testExpr = In $ (In $ (In $ Const 2) `Add` 
            (In $ Const 3)) `Mul` (In $ Const 4)

g = Const 3
k = Mul g (Add (Const 5) g)

-- requires RankNTypes
newtype List a = List (forall b. Monoid b => (a->b)->b)
