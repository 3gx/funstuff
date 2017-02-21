{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -ddump-splices #-}

import Th05
import Language.Haskell.TH
import Language.Haskell.TH.Syntax

data MyData = MyData
    { foo :: String
    , bar :: Int
    }

--emptyShow ''MyData
listFields ''MyData


main = do
  print $ MyData { foo = "bar", bar = 5 }
