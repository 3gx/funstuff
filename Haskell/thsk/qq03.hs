{-# LANGUAGE  QuasiQuotes #-}

-- https://www.schoolofhaskell.com/user/marcin/quasiquotation-101

import QQ03
import Expr2

simpl :: Exp -> Exp
simpl [expr|0 + $x|] = x

main = do
  print$  simpl [expr|0+2|]
  print$  [expr|3+2*3|]
