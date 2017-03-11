module PrettyPrinterBase where

import DataBase


-- Operation for pretty printing

class Exp x => PrettyPrint x
 where
  prettyPrint :: x -> IO ()

instance PrettyPrint Lit
 where
  prettyPrint (Lit i) = putStr (show i)

instance (PrettyPrint l, PrettyPrint r) => PrettyPrint (Add l r) 
 where
  prettyPrint (Add l r) = do prettyPrint l; putStr " + "; prettyPrint r
