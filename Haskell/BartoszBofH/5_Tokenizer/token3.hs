
data Token = Digit | Alpha deriving (Show,Eq)
data Operator = Plus | Minus | Times | Div deriving (Show, Eq)

operator :: Char -> Operator
operator c | c == '+' = Plus
           | c == '-' = Minus
           | c == '*' = Times
           | c == '/' = Div

isDigit :: Char -> Bool
isDigit c = isElem c ['0'..'9']

isElem :: Eq a => a -> [a] -> Bool
isElem x (y:ys) = 
    if x == y
      then True
      else isElem x ys
isElem _ [] = False
    


tokenize :: String -> [Token]
-- tokenize (c : rest) = 
--  if isDigit c
--   then Digit : tokenize rest
--   else Alpha : tokenize rest
tokenize (c : rest) = case (isDigit c) of
      True -> Digit : tokenize rest
      False -> Alpha : tokenize rest
tokenize [] = []

main = do 
  print $ tokenize "passwd123"
  print $ isElem Digit [Digit,Alpha]
  print $ operator '*'
