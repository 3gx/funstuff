-- "Treealize" expression terms

module OperationExtension2 where

import Data.Tree
import DataBase
import DataExtension


class ToTree x
 where
  toTree :: x -> Tree String

instance ToTree Lit
 where
  toTree (Lit i) = Node "Lit" []

instance (Exp x, Exp y, ToTree x, ToTree y) => ToTree (Add x y)
 where
  toTree (Add x y) = Node "Add" [toTree x, toTree y]

instance (Exp x, ToTree x) => ToTree (Neg x)
 where
  toTree (Neg x) = Node "Neg" [toTree x]
