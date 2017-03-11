module Base where

import Functor


-- The functor for a basic expression language

data LangF x = Literal Int

instance Functor LangF
 where
  fmap f (Literal i) = Literal i


-- Fixed point of the language

type Lang = Fix LangF


-- The fold algebra for an interpreter function

evalLangF :: LangF Int -> Int
evalLangF (Literal i) = i


-- The interpreter function

evalLang :: Lang -> Int
evalLang = fold evalLangF
