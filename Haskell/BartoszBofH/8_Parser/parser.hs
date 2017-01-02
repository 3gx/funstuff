-- 

import Data.Char

data Operator = Plus | Minus | Times | Div deriving (Show, Eq)

data Token = TokOp    Operator 
           | TokIdent String
           | TokNum   Double 
           | TokAssign
           | TokLParen
           | TokRParen
           | TokEnd
    deriving (Show, Eq)

operator :: Char -> Operator
operator c | c == '+' = Plus
           | c == '-' = Minus
           | c == '*' = Times
           | c == '/' = Div

tokenize :: String -> [Token]
tokenize [] = []
tokenize (c:cs) 
  | elem c "+-*/" = TokOp (operator c) : tokenize cs
  | c == '=' = TokAssign : tokenize cs
  | c == '(' = TokLParen : tokenize cs
  | c == ')' = TokRParen : tokenize cs
  | isDigit c = number c cs
  | isAlpha c = identifier c cs
  | isSpace c = tokenize cs
  | otherwise = error $ "Cannot tokenize " ++ [c]

identifier c cs = let (str, cs') = span isAlphaNum cs in
                  TokIdent (c:str) : tokenize cs'
number c cs = let (digs, cs') = span isDigit cs in
              TokNum (read (c:digs)) : tokenize cs'

-- parser --
{-| Grammer
 Expression  = Term [+-] Expression 
             | Identifier '=' Expression
             | Term
 Term        = Factor [*/] Term
             | Factor
 Factor      = Number 
             | Identifier 
             | [+-] Factor 
             | '(' Expression ')' 
|-}

-- tree
data Tree = SumNode Operator Tree Tree
          | ProdNode Operator Tree Tree
          | AssignNode String Tree
          | UnaryNode Operator Tree
          | NumNode Double
          | VarNode String
    deriving (Show)


lookAhead :: [Token] -> Token
lookAhead [] = TokEnd
lookAhead (t:ts) = t

accept :: [Token] -> [Token]
accept [] = error "Nothing to accept"
accept (t:ts) = ts

{-|
 Expression  = Term [+-] Expression 
             | Identifier '=' Expression
             | Term
|-}

expression :: [Token] -> (Tree, [Token])
expression toks = 
  let (termTree, toks') = term toks
  in case lookAhead toks' of
    (TokOp op) | elem op [Minus, Plus] ->
      let (exTree, toks'') = expression (accept toks')
      in (SumNode op termTree exTree, toks'')
    TokAssign -> case termTree of
      VarNode str -> 
        let (exTree, toks'') = expression (accept toks')
        in (AssignNode str exTree, toks'')
      _ -> error "Only variables can be assigned to"
    _ -> (termTree, toks')

{-|
 Term        = Factor [*/] Term
             | Factor
|-}

term :: [Token] -> (Tree, [Token])
term toks = 
  let (facTree, toks') = factor toks
  in case lookAhead toks' of
    (TokOp op) | elem op [Times, Div] ->
      let (termTree,toks'') = term (accept toks')
      in (ProdNode op facTree termTree, toks'')
    _ -> (facTree, toks')


{-|
 Factor      = Number 
             | Identifier 
             | [+-] Factor 
             | '(' Expression ')' 
|-}

factor :: [Token] -> (Tree, [Token])
factor toks = 
  case lookAhead toks of
    (TokNum x) -> (NumNode x, accept toks)
    (TokIdent str) -> (VarNode str, accept toks)
    (TokOp op) | elem op [Plus,Minus] ->
      let (facTree, toks') = factor (accept toks)
      in (UnaryNode op facTree, toks')
    TokLParen ->
      let (expTree, toks') = expression (accept toks)
      in
        if  lookAhead toks' /= TokRParen
        then error "Missing right paranthesis"
        else (expTree, accept toks')
    _ -> error $ "Parse error on token: " ++ show toks

parse :: [Token] -> Tree
parse toks = 
  let (tree, toks') = expression toks  
  in if null toks'
     then tree
     else error $ "Left over tokens: " ++ show toks'
              


main = do
  print $ tokenize "x1 = 23/(2+3) "
  print $ tokenize "12 + 24 / x1"
  print $  "12 + 24 / x1"
  print $ (parse.tokenize) "12 + 24 / x1"
  print $ "x1 = -15 / (2 + x2)"
  print $ (parse.tokenize) "x1 = -15 / (2 + x2)"

