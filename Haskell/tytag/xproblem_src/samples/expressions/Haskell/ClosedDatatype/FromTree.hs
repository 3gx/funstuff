module FromTree where

import Data.Tree
import Data

fromTree :: Tree String -> Exp
fromTree (Node "Lit" [Node i []]) = (Lit $ read i)
fromTree (Node "Add" [l,r])       = (Add (fromTree l) (fromTree r))
