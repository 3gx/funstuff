module Extension where

import Functor
import Base


-- The functor for a language extension

data Lang2X x = Plus x x
type Lang2F   = SumF LangF Lang2X

instance Functor Lang2X
 where
  fmap f (Plus x x') = Plus (f x) (f x')


-- Fixed point of the language

type Lang2    = Fix Lang2F


-- The fold algebra for an interpreter function

evalLang2X :: Lang2X Int -> Int
evalLang2X (Plus x x') = x + x'


-- The interpreter function

evalLang2 :: Lang2 -> Int
evalLang2 = fold (bimap evalLangF evalLang2X)
