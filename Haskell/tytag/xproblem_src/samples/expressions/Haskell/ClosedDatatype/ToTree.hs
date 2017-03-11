module ToTree where

import Data.Tree
import Data

toTree :: Exp -> Tree String
toTree (Lit i)   = Node "Lit" [Node (show i) []]
toTree (Add l r) = Node "Add" [toTree l, toTree r]
