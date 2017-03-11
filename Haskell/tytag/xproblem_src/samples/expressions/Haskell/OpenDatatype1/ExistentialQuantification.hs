{-# LANGUAGE ExistentialQuantification #-}

module ExistentialQuantification where

import DataBase
import DataExtension
import OperationExtension1

data AnyExp = forall x. Show x => AnyExp x

instance Show AnyExp
 where
  show (AnyExp x) = show x
