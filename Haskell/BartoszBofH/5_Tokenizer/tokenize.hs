--

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



tokenize :: String -> [Token]
tokenize [] = []
tokenize (c:cs) 
  | elem c "+/-*" = TokOp (operator c) : tokenize cs
  | isDigit c     = TokNum (digitToInt c) : tokenize cs
  | isAlpha c     = TokIdent [c] : tokenize cs
  | isSpace c     = tokenize cs
  | otherwise = error $ "Cannot tokenize " ++ [c]

isDigit :: Char -> Bool
isDigit c = elem c ['0'..'9']

isAlpha :: Char -> Bool
isAlpha c = elem c $ ['a'..'z'] ++ ['A'..'Z']

isSpace :: Char -> Bool
isSpace c = elem c $ " "

digitToInt :: Char -> Int
digitToInt c | isDigit c = fromEnum c - 48

digitToInts :: String -> [Int]
digitToInts [] = []
digitToInts (x:xs) = digitToInt x : digitToInts xs


main = do 
  putStrLn $ showContent token
  print token
  print $ operator '*'
  print $ tokenize "**/+"
  print $ tokenize "1 + 4 / x"
  print $ digitToInts "1234"
