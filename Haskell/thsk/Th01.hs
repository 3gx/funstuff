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
