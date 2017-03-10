-- Chapter 2 from lecture.pdf in 
--   http://okmij.org/ftp/tagless-final/course/index.html
 
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
import Control.Monad

data Exp = Lit Int
         | Neg0 Exp
         | Add Exp Exp
  deriving Show

-- initial embedding
ti1 = Add (Lit 8) (Neg0 (Add (Lit 1) (Lit 2)))

eval :: Exp -> Int
eval (Lit n) = n
eval (Neg0 e) = - eval e
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
view (Neg0 e) = "(-" ++ view e ++ ")"
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
-- ################################# --
--
instance ExpSYM Tree where
  lit n = Node "Lit" [Leaf $ show n]
  neg e = Node "Neg" [e]
  add e1 e2 = Node "Add" [e1,e2]

--instance MulSYM Tree where
--  mul e1 e2 = Node "Mul" [e1,e2]

-- De-serializer
-- fromTree :: (ExpSYM repr, MulSYM repr) => Tree -> repr
-- ################################# --

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
-- ################################# --

fromTreeExt :: (ExpSYM repr) => (Tree -> Either ErrMsg repr) -> 
                                (Tree -> Either ErrMsg repr)
fromTreeExt self (Node "Lit" [Leaf n])   = liftM lit $ safeRead n
fromTreeExt self (Node "Neg" [e])        = liftM neg $ self e
fromTreeExt self (Node "Add" [e1,e2])    = liftM2 add (self e1) (self e2)
fromTreeExt self e = Left $ "Invalid tree: " ++ show e

fix f = f (fix f)
fromTree' = fix fromTreeExt

tf1E_int3 = check_consume thrice . fromTree' $ tf1_tree

-- Extend
-- ################################# --

instance MulSYM Tree where
  mul e1 e2 = Node "Mul" [e1,e2]

instance (MulSYM repr, MulSYM repr') => MulSYM (repr,repr') where
  mul (e11,e12) (e21,e22) = (mul e11 e21, mul e12 e22)

fromTreeExt1 :: (ExpSYM repr, MulSYM repr) => (Tree -> Either ErrMsg repr) -> 
                                             (Tree -> Either ErrMsg repr)
fromTreeExt1 self (Node "Mul" [e1,e2])    = liftM2 mul (self e1) (self e2)
fromTreeExt1 self e = fromTreeExt self e

fromTree1 = fix fromTreeExt1

tfm2'_int3 = check_consume thrice . fromTree1 $ toTree tfm2

-- ################################# --

push_neg0 :: Exp -> Exp
push_neg0 e@Lit{} = e
push_neg0 e@(Neg0 (Lit _)) = e
push_neg0 (Neg0 (Neg0 e)) = push_neg0 e
push_neg0 (Neg0 (Add e1 e2)) = Add (push_neg0 (Neg0 e1)) (push_neg0 (Neg0 e2))
push_neg0 (Add e1 e2)       = Add (push_neg0 e1) (push_neg0 e2)

ti1_norm = push_neg0 ti1

---------------


data Ctx = Pos | Neg

instance ExpSYM repr => ExpSYM (Ctx -> repr) where
  lit n Pos = lit n
  lit n Neg = neg (lit n)
  neg e Pos = e Neg
  neg e Neg = e Pos
  add e1 e2 ctx = add (e1 ctx) (e2 ctx)

push_neg :: (ExpSYM repr) => (Ctx -> repr) -> repr
push_neg e = e Pos

instance MulSYM repr => MulSYM (Ctx -> repr) where
  mul e1 e2 Pos = mul (e1 Pos) (e2 Pos)
  mul e1 e2 Neg = mul (e1 Pos) (e2 Neg)

{-
   e      ::= factor | add factor e
   factor ::= int | neg int   

 addition is right associative
 -}

flata0 :: Exp -> Exp
flata0 e@Lit{} = e
flata0 e@Neg0{} = e
flata0 (Add (Add e1 e2) e3) = flata0 (Add e1 (Add e2 e3))
flata0 (Add e1 e2) = Add e1 (flata0 e2)

norm0 :: Exp -> Exp
norm0  = flata0 . push_neg0

ti3' = (Add ti1 (Neg0 (Neg0 ti1)))
ti3'_view = view ti3'
ti3'_eval = eval ti3'

--------------

data Ctx1 e = LCA e | NonLCA

instance ExpSYM repr => ExpSYM (Ctx1 repr -> repr) where
  lit n NonLCA   = lit n
  lit n (LCA e)  = add (lit n) e
  neg e NonLCA   = neg (e NonLCA)
  neg e (LCA e3) = add (neg (e NonLCA)) e3 -- assume only lits are negated
  add e1 e2 ctx  = e1 (LCA (e2 ctx))

flata :: (Ctx1 repr -> repr) -> repr
flata e = e NonLCA

norm = flata . push_neg

tf3' = (add tf1 (neg (neg tf1)))
tf3'_view = viewExpSYM tf3'
tf3'_eval = evalExpSYM tf3'

--------------------

instance ExpSYM Exp where
  lit = Lit
  neg = Neg0
  add = Add

initialize :: Exp -> Exp
initialize = id

finalize :: ExpSYM repr  => Exp -> repr
finalize (Lit n) = lit n
finalize (Neg0 e) = neg (finalize e)
finalize (Add e1 e2) = add (finalize e1) (finalize e2)

push_neg1 = finalize . push_neg0 . initialize

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
  tfm2'_int3
  print "---------------- Pushing Neg ----------------"
  print $ ti1
  print $ (show ti1_norm ) ++ " = " ++ show ( eval ti1_norm)
  print $ (tf1 :: String)
  print $ (push_neg tf1 :: String)
  print $ (tfm2 :: Int)
  print $ (neg tfm2 :: String)
  print $ (push_neg $ neg tfm2 :: String)
  print $ (push_neg $ neg tfm2 :: Int)
  print "------ flattening ------"
  print ti3'_view
  print ti3'_eval
  print $ view $ norm0 ti3' 
  print tf3'_view
  print tf3'_eval
  print $ viewExpSYM $ norm tf3' 
  print "---- conversion ---"
  print $ initialize tf3'
  print $ viewExpSYM $ finalize $ initialize tf3'
  print $ viewExpSYM $ push_neg1 tf3'

{-  equational reasoning for flata :
 
    add (add (lit 2) (lit 3)) (lit 5) NonLCA
        | e1 = (add (lit 2) (lit 3))
        | e2 = (lit 5)
        | ctx = NonLCA

 == add (lit 2) (lit 3) LCA((lit 5) NonLCA)
       | e1 = (lit 2)
       | e2 = (lit 3)
       | ctx = LCA((lit 5) NonLCA)

 == lit 2 (LCA((lit 3) LCA((lit 5) NonLCA)))
       | n = 2
       | e = ((lit 3) LCA((lit 5) NonLCA))

 = add (lit 2) ((lit 3) LCA((lit 5) NonLCA))
       | e1 = (lit 2)
       | e2 = ((lit 3) LCA((lit 5) NonLCA))
       |            n     (    e          )
       |    = add (lit 3) ((lit 5) NonLCA)
       |               e2a = (lit 5) NonLCA
       |                |         n  
                        |  = lit 5
       |                         
       |    = add (lit 3) (lit 5) 

 = add (lit 2) (add (lit 3) (lit 5)) 
-}
