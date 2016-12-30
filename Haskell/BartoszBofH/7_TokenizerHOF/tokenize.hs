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

-- Scales with O(N^2), N = len(str)
type Accum = (Bool, String, String)
alnums' :: String -> (String,String)
alnums' str = let (_, als, rest) = foldl f (True, [], []) str
              in (als, rest)
  where
    f(True, als, rest) c  | isAlphaNum c = (True, als ++ [c], rest)
                          | otherwise = (False, als, [c])
    f(False, als, rest) c = (False, als, rest ++ [c])


digits :: String -> (String, String)
digits str = digs [] str
  where
    digs acc [] = (acc, [])
    digs acc (c:cs) | isDigit c = digs (c:acc) cs
                    | otherwise = (reverse(acc), c:cs)
      

rev :: String -> String
rev  = foldl (\acc a -> a:acc) []

cpy :: String -> String
cpy = foldr (\a acc -> a:acc) []

span' :: (a->Bool) -> [a] -> ([a],[a])
span' pred str = spanAcc [] str
  where
    spanAcc acc [] = (acc, [])
    spanAcc acc (c:cs) | pred c = spanAcc (c:acc) cs
                       | otherwise = (reverse(acc), c:cs)

span'' :: (a->Bool) -> [a] -> ([a],[a])
span'' pred str = 
  let -- define helper 
    spanAcc acc [] = (acc, [])
    spanAcc acc (c:cs) | pred c = spanAcc (c:acc) cs
                       | otherwise = (reverse(acc), c:cs)
  in
    spanAcc [] str
                                

main = do 
  putStrLn $ showContent token
  print token
  print $ operator '*'
  print $ tokenize "**/+"
  print $ deSpace $ tokenize "1 + 4 / x"
  print $ digitToInts "1234"
  print $ alnums "R2D2+C3Po"
  print $ alnums "a14"
  print $ alnums' "R2D2+C3Po"
  print $ alnums' "a14"
  print $ rev "1234"
  print $ cpy "1234"
  print $ digits "1234abc 5678"
  print $ span' (\c -> isAlphaNum c) "R2D2+C3Po"
  print $ span' isAlphaNum  "R2D2+C3Po"
  print $ span'' isDigit "1234abc 5678"
