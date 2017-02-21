-- http://book.realworldhaskell.org/read/using-parsec.html

import Text.ParserCombinators.Parsec


csvFile = endBy line eol
line = sepBy cell (char ',')
cell = many (noneOf ",\n\r")

eol  =  try (string "\n\r")
    <|> try (string "\r\n")
    <|> string "\n"
    <|> string "\r"
--    <|> fail "Couldn't find EOL"
    <?> "end of line"

parseCSV :: String -> Either ParseError [[String]]
parseCSV input = parse csvFile "(unknown)" input

main = do
  let p =  parseCSV "a,bc, def ,hijk\r\n\n 12,  3456 ,    67899 \n\r"
  print p
  print $  parseCSV "line1\r\nline2\nline3\n\rline4\rline5\n"
  
