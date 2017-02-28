
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}

module QDSL01 (module QDSL01) where

import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Language.Haskell.TH.Quote
import Language.Haskell.TH.Lib

power :: Int -> TExpQ (Float -> Float)
power n | n < 0          = [|| \x -> if x == 0 then 0 else 1 / ($$(power (-n)) x) ||]
        | n == 0         = [|| \x -> 1 ||]
        | n `mod` 2 == 0 = [|| \x -> let y = $$(power (n `div` 2)) x in y*y ||]
        | otherwise      = [|| \x -> x * ($$(power (n-1)) x) ||]


    
  
