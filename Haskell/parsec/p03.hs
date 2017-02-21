-- http://book.realworldhaskell.org/read/using-parsec.html

import Text.ParserCombinators.Parsec

csvFile = endBy line eol
line = sepBy cell (char ',')
cell = quotedCell <|> many (noneOf ",\n\r")

quotedCell = 
    do char '"'
       content <- many quotedChar
       char '"' <?> "quote at end of cell"
       return content

quotedChar =
        noneOf "\""
    <|> try (string "\"\"" >> return '"')

eol =   try (string "\n\r")
    <|> try (string "\r\n")
    <|> string "\n"
    <|> string "\r"
    <?> "end of line"

parseCSV :: String -> Either ParseError [[String]]
parseCSV input = parse csvFile "(unknown)" input

str = "\"Product\",\"Price\"\n"++
      "\"O'Reilly Socks\",10\n"++
      "\"Shirt with \"\"Haskell\"\" text\",20\n"++
      "\"Shirt, \"\"O'Reilly\"\" version\",20\n"++
      "\"Haskell Caps\",15\n"

main = do
  let p =  parseCSV "a,bc, def ,hijk\r\n\n 12,  3456 ,    67899 \n\r"
  print p
  print $  parseCSV "line1\r\nline2\nline3\n\rline4\rline5\n"
  print $  parseCSV "\"This, is, one, big, cell\"\n"
  print $ parseCSV str
