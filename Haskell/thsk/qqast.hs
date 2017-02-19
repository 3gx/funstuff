
{-# LANGUAGE TemplateHaskell #-}

-- http://www.well-typed.com/blog/2014/10/quasi-quoting-dsls/


import QQAst

import Control.Monad
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Language.Haskell.TH.Lib

{-
  prog1 : Prog
  prog1 = [prog|
    var x;
    x := read;
    write (x+x+1)

prog1 = Prog [ 
         Decl "x"
       , Assign "x" Read
       , Write (Add (Add (Var "x') (Var "x")) (Int 1)) 
       ]
 -}

