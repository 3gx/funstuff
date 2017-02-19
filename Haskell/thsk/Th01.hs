module Th01 (module Th01)
             where

import Control.Monad
import Language.Haskell.TH


curryN :: Int -> Q Exp
curryN n = do
  f <- newName "f"
  xs <- replicateM n (newName "x")
  let args = map VarP (f:xs)
      ntup = TupE (map VarE xs)
  return $ LamE args (AppE (VarE f) ntup)

--f= $(curryN 3)



genCurries :: Int -> Q [Dec]
genCurries n = forM [1..n] mkCurryDec
  where mkCurryDec ith = do
          cury <- curryN ith
          let name = mkName $ "curry" ++ show ith
          return $ FunD name [Clause [] (NormalB cury) []]

genId :: Q Exp
genId = [| \x -> x |]

x :: Int
x = 42

static :: Q Exp
static = [| x |]
dynamic :: Q Exp
dynamic = return $ VarE (mkName "x")

data Deriving = Deriving  { tyCon :: Name, tyVar :: Name}

{-
deriveFunctor :: Name -> Q [Dec]
deriveFunctor ty = do  
    (TyConI tyCon) <- reify ty
    (tyConName, tyVars, cs) <- case tyCon of
        DataD    _ nm tyVars cs _ -> return (nm, tyVars, cs)
        NewtypeD _ nm tyVars c  _ -> return (nm, tyVars, [c])
        _                         -> fail "deriveFunctor: tyCon may not be a type synonnym."

    let (KindedTV tyVar StarT) = last tyVars
        instanceType           = conT ''Functor 'appT'
          (foldl apply (conT tyConName) (init tyVars))

    putQ $ Deriving tyConName tyVar
    sequence [instanceD (return []) instanceType [genFmap cs]]
  where
    apply t (PlainTV  name)   = appT t (varT name)
    apply t (KindedTV name _) = appT t (varT name)
 -}    
