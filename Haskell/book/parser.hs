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

-- Sequencing parsers

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


