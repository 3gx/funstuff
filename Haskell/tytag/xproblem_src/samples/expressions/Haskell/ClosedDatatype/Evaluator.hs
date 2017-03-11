module Evaluator where

import Data


-- Operation for expression evaluation

evaluate :: Exp -> Int
evaluate (Lit i)   = i
evaluate (Add l r) = evaluate l + evaluate r
