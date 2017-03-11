module Main where

import Data
import Evaluator
import PrettyPrinter
import FromTree
import ToTree

main = do
          let e1 = Lit 2
          let e2 = Lit 3
          let e3 = Add e1 e2
          let e4 = fromTree $ toTree e3
          prettyPrint e4;
          putStr "\n";
          prettyPrint e3;
          putStr " = ";
          putStr $ show $ evaluate e3;
          putStr "\n";
