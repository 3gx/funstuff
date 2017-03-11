{-# LANGUAGE DeriveDataTypeable #-} 

module DataBase where

import Data.Generics
import Data.Typeable


-- Data variants for literals and addition

data Lit = Lit Int                           deriving (Typeable, Data, Show)
data (Exp l, Exp r) => Add l r = Add l r     deriving (Typeable, Data, Show)


-- The open union of data variants

class Exp x
instance Exp Lit
instance (Exp l, Exp r) => Exp (Add l r)
