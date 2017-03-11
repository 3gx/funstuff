module DataExtension where

import DataBase
import PrettyPrinterBase
import EvaluatorBase


-- Data extension for negation

data Exp x => Neg x = Neg x

instance Exp x => Exp (Neg x)


-- Extending operation for pretty printing

instance PrettyPrint x => PrettyPrint (Neg x)
 where
  prettyPrint (Neg x) = do putStr "(- "; prettyPrint x; putStr ")"


-- Extending operation for expression evaluation

instance Evaluate x => Evaluate (Neg x)
 where
  evaluate (Neg x) = 0 - evaluate x
