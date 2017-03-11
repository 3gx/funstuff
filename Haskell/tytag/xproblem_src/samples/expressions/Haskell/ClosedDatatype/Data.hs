module Data where

-- A data type of expression forms

data Exp = Lit Int | Add Exp Exp
