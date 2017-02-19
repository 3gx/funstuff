{-# LANGUAGE TemplateHaskell #-}

module Th03 (module Th03)
             where

import Control.Monad
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Language.Haskell.TH.Lib

-- Lesson 1

cnst :: Int -> String -> Q Exp
cnst n s = return $ LamE (replicate n WildP) (LitE $ StringL s)

-- Lesson 2

sel :: Int -> Int -> Q Exp
sel n m = do
    x <- newName "x"
    let wilds = replicate m WildP
    return $ LamE (replaceAt (n-1) wilds (VarP x)) $ VarE x
  where
    replaceAt n xs x = take n xs ++ x : drop (n+1) xs

-- Lesson 3


cnst' 0 str = lift str
cnst' n str = do 
       let e = cnst' (n-1) str
       [| \_ -> $e |]

cnst'' 0 str = [| str |]
cnst'' n str = [| \_ -> $(cnst'' (n-1) str) |] 

zip3' = \y1 y2 y3 ->
  case (y1,y2,y3) of
    (x1:xs1,x2:xs2,x3:xs3) -> (x1,x2,x3) : zip3 xs1 xs2 xs3
    (_,_,_) -> []
{-
mkZip :: Int -> Q Exp -> Q Exp
mkZip n name = lamE pYs (caseE (tup eYs) [m1,m2])
  where
    (pXs,  eXs)  = genPE "x"  n
    (pYs,  eYs)  = genPE "y"  n
    (pXSs, eXSs) = genPE "ys" n
    pcons x xs = [p| $x : $xs |]
    b = [| $(tup eXs) : $(apps(name:eXSs)) |]
    m1 = simpleM (ptup (zipWith pcons pXs pXSs)) b
    m2 = simpleM (ptup (copies n pwild))  (con "[]")

genPE s n = do
  ids <- replicateM n (newName s)
  return (map varP ids, map varE ids)  
-}

fibs :: [Integer]
fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

fibsQ :: Q Exp
fibsQ = [| fibs |]

fibQ :: Int -> Q Exp
fibQ n = [| fibs !! n |]

fibSQ :: Int -> Q Exp
fibSQ n = [| take n $ fibs |]
