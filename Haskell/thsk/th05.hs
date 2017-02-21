{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -ddump-splices #-}

-- https://github.com/leonidas/codeblog/blob/master/2011/2011-12-27-template-haskell.md

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
