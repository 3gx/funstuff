
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

-- http://quasimal.com/posts/2012-05-25-quasitext-and-quasiquoting.html

import Language.Haskell.TH 
import Language.Haskell.TH.Quote
import Data.Text (unpack)

import QQ01

a = "yeah" 
b = "maybe" 
person = "jarjar" 
desc   = "best character" 

main = do
  print $ makeChunks  "a b c foobar $doobar $goobar. $asdf"
  print $ unpack [format|Hellow World|]
  let s = unpack [format|$person was $desc, $a, $b, ok maybe not|]
  print s

