-- Ch 13: Monadic parsing


-- type Parser = String -> (Tree,String)
--                |__________|____|_________ input
--                           |____|_________ parse tree
--                                |_________ rest of the string
import Control.Applicative
import Data.Char

newtype Parser a = P { parse :: String -> [(a,String)] }

-- Alternatively
-- newtype Parser a = P ( String -> [(a,String)] )
-- parse :: Parser a -> String -> [(a,String)]
-- parse (P p) inp = p inp

item :: Parser Char
item = P (\inp -> case inp of []     -> []
                              (x:xs) -> [(x,xs)])

-- > parse item ""
-- []
-- > parse item "abc"
-- [('a',"bc")]

-- 13.4 Sequencing parsers

instance Functor Parser where
  fmap f (P p) = P (\inp -> case p inp of [] -> []
                                          [(x,out)] -> [(f x, out)])

-- > parse (fmap toUpper item) "abc"
-- [('A',"bc")]
-- > parse (fmap toUpper item) ""
-- []


instance Applicative Parser where
  pure v = P (\inp -> [(v,inp)])
  (P f) <*> px = 
       P (\inp -> case f inp of [] -> []
                                [(g,out)] -> parse (fmap g px) out)
-- > parse (pure 1) "abc"
-- [(1,"abc")]

three :: Parser (Char, Char)
three = pure g <*> item <*> item <*> item
            where g  x y z = (x,z)

-- > parse three "abcdef"
-- [(('a','c'),"def")]
-- > parse three "abc"
-- [(('a','c'),"")]
-- > parse three "ab"
-- []

instance Monad Parser where
  return v = P(\inp -> [(v,inp)])
  (P x) >>= f = P (\inp -> case x inp of  
                                [] -> []
                                [(v,out)] -> parse (f v) out)

mthree :: Parser (Char, Char)
mthree = do
  a <- item
  item
  b <- item
  return (a,b)
     
-- > parse mthree "abcdef"
-- [(('a','c'),"def")]
-- > parse mthree "abc"
-- [(('a','c'),"")]
-- > parse mthree "ab"
-- []

-- 13.5 Making choices

instance Alternative Parser where
  empty = P (\inp -> [])
  (P p) <|> (P q) = P (\inp -> case p inp of  [] -> q inp
                                              [(v,out)] -> [(v,out)])

-- > parse empty "abc"
-- []
-- > parse (item  <|> return 'd') "abc"
-- [('a',"bc")]
-- > parse (empty  <|> return 'd') "abc"
-- [('d',"abc")]
-- > parse (three <|> return ('d','c')) "abcdef"
-- [(('a','c'),"def")]
-- > parse (three <|> return ('d','c')) "ab"
-- [(('d','c'),"ab")]

-- 13.6 Derived primitives

sat :: (Char -> Bool) -> Parser Char
sat p = do 
      x <- item
      if (p x) then return x else empty

digit :: Parser Char
digit = sat isDigit

lower :: Parser Char
lower = sat isLower

upper :: Parser Char
upper = sat isUpper

letter :: Parser Char
letter = sat isAlpha

alphanum :: Parser Char
alphanum = sat isAlphaNum

char :: Char -> Parser Char
char x = sat (== x)

-- > parse (char 'a') "abc"
-- [('a',"bc")]
-- > parse (char 'a') "xabc"
-- []

string :: String -> Parser String
string [] = return []
string (x:xs) = do 
    char x
    string xs
    return (x:xs)

--  (string "xab") "xabc"
--  [("xab","c")]
--  > parse (string "xaa") "xabc"
--  []

-- > parse (many digit) "1123abc"
-- [("1123","abc")]
-- > parse (many digit) "a1123abc"
-- [("","a1123abc")]
-- > parse (some digit) "a1123abc"
-- []

ident :: Parser String
ident = do 
    x <- lower
    xs <- many alphanum
    return (x:xs)

nat :: Parser Int
nat = do
  xs <- some digit
  return (read xs)

space :: Parser ()
space = do
  many (sat isSpace)
  return ()

int :: Parser Int
int = do char '-'
         n <- nat
         return (-n)
      <|> nat

-- > parse ident "abc def"
-- [("abc"," def")]
-- > parse ident "Abc def"
-- []
-- > parse space " abc"
-- [((),"abc")]
-- > parse space "  abc"
-- [((),"abc")]
-- > parse nat "123 abc"
-- [(123," abc")]
-- > parse nat "-123 abc"
-- []
-- > parse int "-123 abc"
-- [(-123," abc")]
-- > parse int "123 abc"
-- [(123," abc")]


-- 13.7 Handling spaces

token :: Parser a -> Parser a 
token p = do space
             v <- p
             space
             return v 

identifier :: Parser String
identifier = token ident

natural :: Parser Int
natural = token nat

integer :: Parser Int
integer = token int

symbol :: String -> Parser String
symbol xs = token (string xs)

nats :: Parser [Int]
nats = do symbol "["
          n <- natural
          ns <- many (do {symbol ","; natural})
          symbol "]"
          return (n:ns)

--  > parse nats "[1, 2,  3, 4]"
--  [([1,2,3,4],"")]
--  > parse nats "[1, 2,  3, 4,]"
--  []


-- 13.8 Arithmetic expressions

{-
 Grammar
---------
   expr ::= term ( + expr | eps)
   term ::= factor (*term | esp)
   factor ::= ( expr ) | nat
   nat ::= 0 |1 |2 |...
-}

expr :: Parser Int
expr = do t <- term
          do symbol "+"
             e <- expr
             return (t + e)
             <|> return t

term :: Parser Int
term = do f <- factor
          do symbol "*"
             t <- term
             return (f*t)
             <|> return f

factor :: Parser Int
factor = do symbol "("
            e <- expr
            symbol ")"
            return e
         <|> natural


eval :: String -> Int
eval xs = case (parse expr xs) of
              [(n,[])] -> n
              [(_,out)] -> error ("Unused input " ++ out)
              [] -> error "Invalid input"

