{-# LANGUAGE TemplateHaskell #-}

-- http://web.archive.org/web/20100703060841/http://www.haskell.org/bz/th3.htm

import Th03

import Control.Monad
import Control.Monad
import Language.Haskell.TH
import Language.Haskell.TH.Syntax

main = do
  runQ [| 1 |] >>= print
  runQ [| \x _ -> x |] >>= print
  runQ [| \x _ -> x |] >>= putStrLn.pprint
  runQ (cnst 1 "x") >>= print
  runQ (cnst 2 "str") >>= print
  runQ (cnst 3 "hey") >>= putStrLn.pprint
  runQ (cnst 3 "hey") >>= print
  runQ [| \_ _ _ -> "hey" |] >>= print
  runQ (sel 1 3) >>= putStrLn.pprint
  runQ (sel 2 4) >>= putStrLn.pprint
  runQ (return$ VarE (mkName "x")) >>= putStrLn.pprint
  runQ (return$ VarP (mkName "x")) >>= putStrLn.pprint
  runQ (tupE [dyn "x", dyn "x"]) >>= putStrLn.pprint
  runQ (cnst' 3 "hey!") >>= putStrLn.pprint
  runQ (cnst'' 3 "hey!") >>= putStrLn.pprint
  print $ zip3' [1,2,3] "abcdef" [32.0,4.0]


