{-# LANGUAGE QuasiQuotes, TemplateHaskell #-}
{-# OPTIONS_GHC -Wall #-}

-- http://www.well-typed.com/blog/2014/10/quasi-quoting-dsls/

import QQAst

import Language.Haskell.TH.Syntax

prog1 :: Prog
prog1 = [prog|
    var x ;
    x := read ;
    write (x + x + 1)
  |]

prog2 :: VarName -> Integer -> Prog
prog2 y n = [prog|
    var x ;
    x := read ;
    write (x + y + n)
  |]

optimize :: Expr -> Expr
optimize [expr| a + n - m |] | n == m = optimize a
optimize other = other

test1 :: IO ()
test1 = intIO $ intProg prog1

test2 :: IO ()
test2 = intIO $ intProg (prog2 "x" 2)

test3 :: IO ()
test3 = print . optimize =<< parseIO parseExpr =<< getLine

test4 :: Lift a => a -> Q Exp
test4 x = [| id x |]
