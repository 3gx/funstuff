module OperationExtension3 where

import ColonT
import DataBase
import DataExtension


-- Showing types concisely

instance ShowType Lit
 where
  t _ = "Exp"

instance (Exp x, Exp y) => ShowType (Add x y)
 where
  t _ = "Exp"

instance Exp x => ShowType (Neg x)
 where
  t _ = "Exp"
