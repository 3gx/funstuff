module EvaluatorBase where

import DataBase


-- Operation for expression evaluation

class Exp x => Evaluate x 
 where
  evaluate :: x -> Int

instance Evaluate Lit
 where
  evaluate (Lit i) = i

instance (Evaluate l, Evaluate r) => Evaluate (Add l r)
 where 
  evaluate (Add l r) = evaluate l + evaluate r
