{-# LANGUAGE TemplateHaskell #-}

-- http://web.archive.org/web/20100703060856/http://www.haskell.org/bz/thdoc.htm

import Th02

import Control.Monad
import Control.Monad
import Language.Haskell.TH
import Language.Haskell.TH.Syntax

{-
  Exp  - expressions
  Pat  - patterns
  Lit  - literals
  Dec  - declarations
  Type - data types
  ...
 -}

-- > :t $(tupleReplicate 3)
-- $(tupleReplicate 3) :: t -> (t, t, t)

varx = VarE (mkName "x")
patx = VarP (mkName "x")
str = LitE (StringL "str")
tuple = TupE [varx, str]
lam = LamE [patx] tuple

tupleQ :: ExpQ
tupleQ = return tuple

patxQ :: PatQ
patxQ = return patx

lamQ ::ExpQ
lamQ = lamE [patxQ] tupleQ

data T = A Int String | B Integer | C | DD Double
$(deriveShow ''T)


k= [A 1 "s", B 2, C, DD 42]  -- prints exactly <<[A 1 "s",B 2,C]>>

main = do
  print $ lam
  print $ $(tupleReplicate 3) 42
  print $ $(tupleReplicate 3) 42
  runQ lamQ >>= print 
  runQ (summ 2) >>= print
  print $ $(printf "Str %s and dbl %d") "test" 42
  putStrLn $ $(printf "Error in file %s line %d: %s") "io.cpp" 325 "printer not found" 

-- > print k
-- [A 1 "s",B 2,C,DD 42.0]
-- > k
-- [A 1 "s",B 2,C,DD 42.0]


