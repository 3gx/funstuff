
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

import Language.Haskell.TH 
import Language.Haskell.TH.Quote
import Data.Text (unpack)

import QQ01

a = "yeah" 
b = "maybe" 
person = "jarjar" 
desc   = "best character" 
s = unpack [format|$person was $desc, $a, $b, ok maybe not|]

main = do
  print $ makeChunks  "a b c foobar $doobar $goobar. $asdf"
  print $ unpack [format|Hellow World|]
  print s

