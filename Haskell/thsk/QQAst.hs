{-# LANGUAGE TemplateHaskell #-}

module QQAst (module QQAst)
             where

import Control.Monad
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Language.Haskell.TH.Lib

type VarName = String

data Expr = Var VarName
          | Add Expr Expr 
          | Sub Expr Expr
          | Int Integer
          | Read
    deriving (Data, Typeable, Show, Eq)

data Cmd = Write Expr
         | Assign VarName Expr
         | Decl VarName
  deriving (Data, Typeable, Show)

data Prog = Prog [Cmd] deriving (Data, Typeable, Show)
