-- Ch 13: Monadic parsing


-- type Parser = String -> (Tree,String)
--                |__________|____|_________ input
--                           |____|_________ parse tree
--                                |_________ rest of the string
import Control.Applicative
import Data.Char

newtype Parser a = P (String -> [(a,String)])

parse :: Parser a -> String -> [(a,String)]
parse (P p) inp = p inp

item :: Parser Char
item = P (\inp -> case inp of []     -> []
                              (x:xs) -> [(x,xs)])

-- > parse item ""
-- []
-- > parse item "abc"
-- [('a',"bc")]
--

