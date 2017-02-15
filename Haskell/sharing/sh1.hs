-- https://jtobin.io/sharing-in-haskell-edsls

{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TypeFamilies #-}

import Data.StableMemo
-- import Control.Applicative
import Data.Reify hiding (Graph)
import qualified Data.Reify as Reify
import System.IO.Unsafe

import Data.Graph
import Data.Maybe

naiveTree :: (Eq a, Num a) => a -> a
naiveTree 0 = 1
naiveTree n = naiveTree (n-1) + naiveTree (n-1)

tree :: (Eq a, Num a) => a -> a
tree 0 = 1
tree n = let shared = tree (n-1)
         in  shared + shared

data Expr = Lit Int
          | Add Expr Expr
    deriving (Eq, Ord, Show)

instance Num Expr where
  fromInteger = Lit . fromInteger
  (+) = Add
  (Lit a) - (Lit b) = Lit (a-b)

eval :: Expr -> Int
eval (Lit d) = d
eval (Add e0 e1) = eval e0 + eval e1

-- > tree 2 :: Expr
-- Add (Add (Lit 1) (Lit 1)) (Add (Lit 1) (Lit 1))
-- > naiveTree  2 :: Expr
-- Add (Add (Lit 1) (Lit 1)) (Add (Lit 1) (Lit 1))


memoEval :: Expr -> Int
memoEval = go where
  go = memo eval
  eval (Lit i) = i
  eval (Add e0 e1) = go e0 + go e1

-- > memoEval (tree 50 :: Expr)
-- 1125899906842624

data ExprF e = LitF Int
             | AddF e e
        deriving (Eq, Ord, Show, Functor)

instance MuRef Expr where
  type DeRef Expr = ExprF
  mapDeRef f (Add e0 e1) = AddF <$> f e0 <*> f e1
  mapDeRef _ (Lit v)     = pure (LitF v)

naiveEval :: Expr -> Int
naiveEval expr = gEval reified where
  reified = unsafePerformIO $ reifyGraph expr
  gEval (Reify.Graph env r) = go r where
    go j = case lookup j env of
      Just (AddF a b) -> go a + go b
      Just (LitF d ) -> d
      Nothing -> 0

-- > naiveEval (tree 4)
-- 16
-- > naiveEval (tree 50)
-- [hang] 

graphEval :: Expr -> Int
graphEval expr = consume reified where
  reified = unsafePerformIO $ toGraph <$> reifyGraph expr
  toGraph (Reify.Graph env _) = graphFromEdges . map toNode $ env
  toNode (j, AddF a b) = (AddF a b, j, [a,b])
  toNode (j, LitF d)   = (LitF d, j, [])

consume :: Eq a => (Graph, Vertex -> (ExprF a, a, b), c) -> Int
consume (g, vmap, _) = go (reverse . topSort $ g) [] where
  go [] acc = snd $ head acc 
  go (v:vs) acc = let nacc = evalNode (vmap v) acc : acc
                  in  go vs nacc

evalNode :: Eq a => (ExprF a, b, c) -> [(a,Int)] -> (b,Int)
evalNode (LitF d, k, _) _ = (k, d)
evalNode (AddF a b, k, _) l = 
  let v = fromJust ((+) <$> lookup a l <*> lookup b l)
  in  (k,v)
