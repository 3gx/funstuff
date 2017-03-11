module PrettyPrinter where

import Data


-- Operation for pretty printing

prettyPrint :: Exp -> IO ()
prettyPrint (Lit i) = putStr (show i)
prettyPrint (Add l r) = do prettyPrint l; putStr " + "; prettyPrint r
