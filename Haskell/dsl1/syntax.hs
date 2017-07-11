{-# LANGUAGE GADTs #-}

module Syntax where

import Control.Monad

type Name = String
data PExpr = PVar Name
          | PLit Lit
          | PApp PExpr PExpr 
          | PLam Name PExpr
    deriving Show

data Lit = LInt Int
         | LBool Bool
    deriving Show

data Program a where
  Var :: Name -> Program Name
  App :: Program a -> Program b  -> Program ()
  Lam :: Name -> Program a -> Program ()
  Lit :: Lit -> Program Lit
  
  Return :: a -> Program a
  Bind   :: Program a -> (a -> Program b) -> Program b

instance Monad Program where
  return = Return
  (>>=)  = Bind

instance Applicative Program where
  pure = return
  mf <*> mx = do
    f <- mf
    x <- mx
    return $ f x

instance Functor Program where
  fmap f = liftM f

var = Var
app = App
lam = Lam
lint = Lit . LInt
lbool = Lit . LBool

expr = do
  let e1 = lam "x" (var "x")
  let e2 = lam "y" (var "y")
  let e3 = app e1 e2  
 
  app e3 (lint 3)


runCompile :: Program a -> PExpr
runCompile prg = snd $ compile prg

compile :: Program a -> (a, PExpr)
compile (Var x) = (x, PVar x)
compile (Lam x prg) = ((), PLam x p)
  where
    (_, p) = compile prg
compile (App e1 e2) = ((), PApp pe1 pe2)
  where
    (_, pe1) = compile e1
    (_, pe2) = compile e2
compile (Lit l) = (l, PLit l)

