module Main where

import DataBase
import PrettyPrinterBase
import EvaluatorBase
import DataExtension
import OperationExtension1
import OperationExtension2
import ExistentialQuantification


main = do
          -- Sample terms
          let e1 = Lit 2
          let e2 = Lit 3
          let e3 = Add e1 e2

          -- Compute e3
          putStrLn $ show e3
          print $ toTree e3
          prettyPrint e3
          putStr " = "
          putStrLn $ show $ evaluate e3
