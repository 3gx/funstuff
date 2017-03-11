-- Show expressions in prefix notation

module OperationExtension1 where

import DataBase
import DataExtension


instance Show Lit
 where
  show (Lit i) = "Lit " ++ show i

instance (Exp x, Exp y, Show x, Show y) => Show (Add x y)
 where
  show (Add x y) = "Add (" ++ show x ++ ") (" ++ show y ++ ")"

instance (Exp x, Show x) => Show (Neg x)
 where
  show (Neg x) = "Neg (" ++ show x ++ ")"
