--

import Data.Char

data Operator = Plus | Minus | Times | Div deriving (Show,Eq)

opToChar :: Operator -> Char
-- opToChar = undefined

opToChar Plus  = '+'
opToChar Minus = '-'
opToChar Times = '*'
opToChar Div   = '/'

opToStr :: Operator -> String

opToStr Plus  = "+"
opToStr Minus = "-"
opToStr Times = "*"
opToStr Div   = "/"

data Token = TokOp Operator 
           | TokIdent String
           | TokNum Int
           | TokSpace
        deriving (Show,Eq)

showContent :: Token -> String
showContent (TokOp op) = opToStr op
showContent (TokIdent str) = str
showContent (TokNum i) = show i

token :: Token
token = TokIdent "x"

operator :: Char -> Operator
operator c | c == '+' = Plus
           | c == '-' = Minus
           | c == '*' = Times
           | c == '/' = Div



tokenizeChar :: Char -> Token
tokenizeChar c
  | elem c "+/-*" = TokOp (operator c) 
  | isDigit c     = TokNum (digitToInt c) 
  | isAlpha c     = TokIdent [c] 
  | isSpace c     = TokSpace
  | otherwise = error $ "Cannot tokenizeChar " ++ [c]

tokenize :: String -> [Token]
tokenize = map tokenizeChar

-- isDigit :: Char -> Bool
-- isDigit c = elem c ['0'..'9']

-- isAlpha :: Char -> Bool
-- isAlpha c = elem c $ ['a'..'z'] ++ ['A'..'Z']

-- isSpace :: Char -> Bool
-- isSpace c = elem c $ " "

-- digitToInt :: Char -> Int
-- digitToInt c | isDigit c = fromEnum c - 48

digitToInts :: String -> [Int]
digitToInts = map digitToInt

deSpace :: [Token] -> [Token]
deSpace = filter (\t -> t /= TokSpace)

alnums :: String -> (String, String)
alnums str = als "" str
  where
    als acc [] = (acc, [])
    als acc (c:cs) | isAlphaNum c = als (c:acc) cs
                   | otherwise = (reverse(acc), c:cs)
      


main = do 
  putStrLn $ showContent token
  print token
  print $ operator '*'
  print $ tokenize "**/+"
  print $ deSpace $ tokenize "1 + 4 / x"
  print $ digitToInts "1234"
  print $ alnums "R2D2+C3Po"
