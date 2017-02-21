-- http://book.realworldhaskell.org/read/using-parsec.html

import Control.Applicative
import Text.ParserCombinators.Parsec
import Numeric (readHex)

p_hex :: CharParser () Char
p_hex = do
  char '%'
  a <- hexDigit
  b <- hexDigit
  let ((d, _):_) = readHex [a,b]
  return . toEnum $ d

a_hex :: CharParser () Char
a_hex = hexify <$> (char '%' *> hexDigit)  <*> hexDigit 

a_hex3 :: CharParser () Char
a_hex3 = hexify3 <$> (char '%' *> hexDigit)  <*> hexDigit <*> hexDigit

hexify :: Char -> Char -> Char
hexify a b = toEnum . fst . head . readHex $ [a,b]

hexify3 :: Char -> Char -> Char -> Char
hexify3 a b c = toEnum . fst . head . readHex $ [a,b,c]

main = do
  print $ parse p_hex "" "%80"
  print $ parse a_hex "" "%ab"
  print $ parse a_hex3 "" "%abc"
