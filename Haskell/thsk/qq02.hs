-- https://www.schoolofhaskell.com/user/marcin/quasiquotation-101

import QQ02

import Text.ParserCombinators.Parsec

test1 = parse pExp "test1" "1 - 2 - 3 * 4 "

test2 :: Exp
test2 = (2+2)*4

main = do
  print test1
  print $ parse pExp "testExp" "1+2*3"
  print test2
  print $ simpl (0 + 3)

