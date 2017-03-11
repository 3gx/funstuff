module Main where

import DataBase
import PrettyPrinterBase
import EvaluatorBase
import DataExtension
import OperationExtension3
import ColonT
import IO


main = do
          -- Sample terms
          let e1 = Lit 2
          let e2 = Lit 3
          let e3 = Add e1 e2

          -- Compute e3
          prettyPrint e3
          putStr " = "
          putStrLn $ show $ evaluate e3

          -- Illustrate ":t" replacement
          putStrLn $ t e3

          -- Illustrate de-/serialization
          putStrLn $ show $ tree2exp (toTree e3)
