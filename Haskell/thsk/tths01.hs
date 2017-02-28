-- type templated haskell
-- http://gmainland.blogspot.com/2013/05/type-safe-runtime-code-generation-with.html


{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}

import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Language.Haskell.TH.Quote
import Language.Haskell.TH.Lib

import TThs01

main = do
  e <- runQ $ unMeta $ mkPow 3
  print $ show e
  (runQ $ unMeta $ mkPow 3) >>= putStrLn.pprint
  exp <- runQ $ mkPow1 3
  print $ unType exp
  putStrLn.pprint.unType $ exp
  let f1 = $(unMeta $ mkPow 3)
  let f2 = $$(mkPow1 3)
  print $ f1 5
  print $ f2 5
  


