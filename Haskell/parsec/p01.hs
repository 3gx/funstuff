-- http://book.realworldhaskell.org/read/using-parsec.html

import Text.ParserCombinators.Parsec

{- A CSV file contains 0 or more lines, each of which is terminated by the EOL
  characterl (eol) 
-}
csvFile :: GenParser Char st [[ String ]]
csvFile = do
    result <- many line
    eof
    return result

-- Each line contains 1 or more cells, separated by a comma
line :: GenParser Char st [String]
line = do
    result <- cells
    eol
    return result

-- Build up a list of cells. Try to parse first cell, then figure out what ends
-- the cell.
cells :: GenParser Char st [String]
cells = do
   first <- cellContent
   next  <- remainingCells
   return (first : next)

-- The cell either ends with a comman, indicating that 1 or more cells follow,
-- or it doesn't, indicating that we're a tthe end of the cell for this line
remainingCells :: GenParser Char st [String]
remainingCells  =  (char ',' >> cells) 
               <|> (return [])

-- Each cell contains 0 or more characters, which must not be a comma or EOL
cellContent :: GenParser Char st String
cellContent = many (noneOf ",\n")

-- The end of line character is \n
eol :: GenParser Char st Char
eol = char '\n'

parseCSV :: String -> Either ParseError [[String]]
parseCSV input = parse csvFile "(unknown)" input

main = do
  let p =  parseCSV "a,bc, def ,hijk\n 12,  3456 ,    67899 \n"
  print p
  
