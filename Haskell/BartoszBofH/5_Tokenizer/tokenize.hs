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

main = do 
  putStrLn $ showContent token
  print token
