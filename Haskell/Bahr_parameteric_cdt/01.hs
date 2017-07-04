{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DeriveFunctor #-}

type VarT = String

data Lam  a = Lam VarT a deriving Functor
data Var  a = Var VarT deriving Functor
data App  a = App a a deriving Functor
data Lit  a = Lit Int deriving Functor
data Plus a = Plus a a deriving Functor
data Let  a = Let VarT a a  deriving Functor
data Err  a = Err deriving Functor

infixr :+:
data (f :+: g) a = Inl (f a) | Inr (g a)
instance (Functor f, Functor g) => Functor (f :+: g) where
  fmap f (Inl x) = Inl (fmap f x)
  fmap f (Inr x) = Inr (fmap f x)

type Sig  = Lam :+: Var :+: App :+: Lit :+: Plus :+: Err :+: Let
type Sig' = Lam :+: Var :+: App :+: Lit :+: Plus :+: Err 

data Term f = In { out :: f (Term f) }

class sub :<: sup where
  inj :: sub a-> sup a
  proj :: sup a -> Maybe (sub a)

instance Functor f => f :<: f where
  inj = id
  proj = undefined

instance (Functor f, Functor g) => f :<: (f :+: g) where
  inj = Inl
  proj = undefined

instance (Functor f, Functor g, Functor h, f :<: g) => f :<: (h :+: g) where
  inj = Inr . inj
  proj = undefined

inject :: (g :<: f) => g (Term f) -> Term f
inject = In . inj

project :: (g :<: f) => Term f -> Maybe (g (Term f))
project = proj . out

iPlus :: (Plus :<: f) => Term f -> Term f -> Term f
iPlus x y = inject (Plus x y)

iLet :: (Let :<: f) => VarT -> Term f -> Term f -> Term f
iLet name x y = inject (Let name x y)

iLit :: (Lit :<: f) => Int -> Term f
iLit x = inject (Lit x)

iLam :: (Lam :<: f) => VarT -> Term f -> Term f 
iLam name expr = inject (Lam name expr)

iVar :: (Var :<: f) => VarT -> Term f 
iVar name = inject (Var name)

iApp :: (App :<: f) => Term f -> Term f -> Term f
iApp e1 e2 = inject (App e1 e2)

e :: Term Sig
e = iLet "x" (iLit 2) ((iLam "y" (iVar "y" `iPlus` iVar "x")) `iApp` iLit 3)

type Alg f a = f a -> a
cata :: Functor f => Alg f a -> Term f -> a
cata phi = phi . fmap (cata phi) . out

class Pretty f where
  fpretty :: Alg f String

instance (Pretty f, Pretty g) => Pretty (f :+: g) where
  fpretty (Inl x ) = fpretty x
  fpretty (Inr x ) = fpretty x

pretty :: (Functor f, Pretty f) => Term f -> String
pretty = cata fpretty

instance Pretty Lam where 
  fpretty (Lam x e) = "(\\" ++ x ++ ". " ++ e ++ ")"

instance Pretty Var where 
  fpretty (Var x) = x

instance Pretty App where 
  fpretty (App e1 e2) = "(" ++ e1 ++ " " ++ e2 ++ ")"

instance Pretty Lit where 
  fpretty (Lit n) = show n

instance Pretty Plus where 
  fpretty (Plus e1 e2) = "(" ++ e1 ++ " + " ++ e2 ++ ")"

instance Pretty Let where 
  fpretty (Let x e1 e2) = "(let " ++ x ++ " + " ++ e1 ++ " in " ++ e2 ++ ")"

instance Pretty Err where 
  fpretty Err = "error"

prettye = pretty e


