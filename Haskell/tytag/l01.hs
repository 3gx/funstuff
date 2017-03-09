{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
import Control.Monad

data Exp = Lit Int
         | Neg Exp
         | Add Exp Exp
  deriving Show

-- initial embedding
ti1 = Add (Lit 8) (Neg (Add (Lit 1) (Lit 2)))

eval :: Exp -> Int
eval (Lit n) = n
eval (Neg e) = - eval e
eval (Add e1 e2) = eval e1 + eval e2

type Repr = Int

lit' :: Int -> Repr
lit' n = n

neg' :: Repr -> Repr
neg' e = -e

add' :: Repr -> Repr -> Repr
add' e1 e2 = e1 + e2

-- final embedding
tf1' = add' (lit' 8) (neg' (add' (lit' 1) (lit' 2)))

view :: Exp -> String
view (Lit n) = show n
view (Neg e) = "(-" ++ view e ++ ")"
view (Add e1 e2) = "(" ++ view e1 ++ " + " ++ view e2 ++ ")"

class ExpSYM repr where
  lit :: Int -> repr
  neg :: repr -> repr
  add :: repr -> repr -> repr

instance ExpSYM Int where
  lit n = n
  neg e = -e
  add e1 e2 = e1 + e2

instance ExpSYM String where
  lit n = show n
  neg e = "(-" ++ e ++ ")"
  add e1 e2 = "(" ++ e1 ++ " + " ++ e2 ++ ")"

tf1 = add (lit 8) (neg (add (lit 1) (lit 2)))

evalExpSYM :: Int -> Int
evalExpSYM = id

viewExpSYM :: String -> String
viewExpSYM = id

til1 = [Lit 1, Add (Lit 1) (Lit 3)]
tif1 = [lit 1, add (lit 1) (lit 3)]

class MulSYM repr where
  mul :: repr -> repr -> repr

tfm1 = add (lit 7) (neg (mul (lit 1) (lit 2)))
tfm2 = mul (lit 7) tf1

instance MulSYM Int where
  mul e1 e2 = e1 * e2

instance MulSYM String where
    mul e1 e2 = "(" ++ e1 ++ " * " ++ e2 ++ ")"

data Tree = Leaf String
          | Node String [Tree]
        deriving (Eq, Read, Show)

-- Serializer
instance ExpSYM Tree where
  lit n = Node "Lit" [Leaf $ show n]
  neg e = Node "Neg" [e]
  add e1 e2 = Node "Add" [e1,e2]

--instance MulSYM Tree where
--  mul e1 e2 = Node "Mul" [e1,e2]

-- De-serializer
-- fromTree :: (ExpSYM repr, MulSYM repr) => Tree -> repr

type ErrMsg = String

safeRead :: Read a => String -> Either ErrMsg a
safeRead s = case reads s of
             [(x,"")] -> Right x
             _        -> Left $ "Read_error: " ++ s

fromTree :: (ExpSYM repr) => Tree -> Either ErrMsg repr
fromTree (Node "Lit" [Leaf n])   = liftM lit $ safeRead n
fromTree (Node "Neg" [e])        = liftM neg $ fromTree e
fromTree (Node "Add" [e1,e2])    = liftM2 add (fromTree e1) (fromTree e2)
  
tf1_tree = (tf1 :: Tree)
tf1_tree1 = "Node \"Add\" [Node \"Lit\" [Leaf \"8\"],Node \"Neg\" [Node \"Add\" [Node \"Lit\" [Leaf \"1\"],Node \"Lit\" [Leaf \"2\"]]]]"
tf1_d' = fromTree tf1_tree

instance (ExpSYM repr, ExpSYM repr') => ExpSYM (repr,repr') where
  lit x = (lit x, lit x)
  neg (e1, e2) = (neg e1, e2)
  add (e11, e12) (e21,e22) = (add e11 e21, add e12 e22)

duplicate :: (ExpSYM repr, ExpSYM repr') => (repr, repr') -> (repr, repr')
duplicate = id

check_consume f (Left e) = putStrLn $ "Error: " ++ e
check_consume f (Right x) = f x

dup_consume ev x = print (ev x1) >> return x2
  where (x1,x2) = duplicate x

toTree :: Tree -> Tree
toTree = id

thrice x = dup_consume evalExpSYM x >>= dup_consume viewExpSYM >>= print . toTree
tf1'_int3 = check_consume thrice . fromTree $ tf1_tree

-- extensibility, use open-recursion style

fromTreeExt :: (ExpSYM repr) => (Tree -> Either ErrMsg repr) -> 
                                (Tree -> Either ErrMsg repr)
fromTreeExt self (Node "Lit" [Leaf n])   = liftM lit $ safeRead n
fromTreeExt self (Node "Neg" [e])        = liftM neg $ self e
fromTreeExt self (Node "Add" [e1,e2])    = liftM2 add (self e1) (self e2)
fromTreeExt self e = Left $ "Invalid tree: " ++ show e

fix f = f (fix f)
fromTree' = fix fromTreeExt

tf1E_int3 = check_consume thrice . fromTree' $ tf1_tree

main = do
  print $ evalExpSYM tf1
  print $ viewExpSYM tf1
  print $ (tf1 :: String)
  print $ til1
  print $ (tif1 :: [String])
  print $ (tif1 :: [Int])
  print $ map eval til1
  print $ map viewExpSYM tif1
  print $ map evalExpSYM tif1
  print $ (tfm2 :: String)
  print $ (tfm2 :: Int)
  print $ (tf1 :: String)
  print $ (tf1 :: Int)
  print $ (tf1 :: Tree)
  print tf1_tree
  let tf1' = fromTree tf1_tree
  case tf1' of
     Left e -> putStrLn $ "Error: " ++ e
     Right e-> do print $ (e :: Int)
  tf1'_int3
  tf1E_int3
