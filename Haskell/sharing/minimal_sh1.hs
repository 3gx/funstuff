{-# OPTIONS_GHC -fno-warn-missing-methods #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TypeFamilies #-}

module Minimal where

import Control.Applicative
import Data.Graph
import Data.Maybe
import Data.Reify hiding (Graph)
import qualified Data.Reify as Reify
import Data.StableMemo
import System.IO.Unsafe

data Expr =
    Lit Int
  | Add Expr Expr
  deriving (Eq, Show)

instance Num Expr where
  fromInteger = Lit . fromInteger
  (+)         = Add

data ExprF e =
    LitF Int
  | AddF e e
  deriving (Eq, Show, Functor)

instance MuRef Expr where
  type DeRef Expr = ExprF
  mapDeRef f (Add e0 e1) = AddF <$> f e0 <*> f e1
  mapDeRef _ (Lit d)     = pure (LitF d)

eval :: Expr -> Int
eval = graphEval

naiveEval :: Expr -> Int
naiveEval expr = gEval reified where
  reified = unsafePerformIO $ reifyGraph expr
  gEval (Reify.Graph env r) = go r where
    go j = case lookup j env of
      Just (AddF a b) -> go a + go b
      Just (LitF d)   -> d
      Nothing         -> 0

memoEval :: Expr -> Int
memoEval = go where
  go = memo eval'
  eval' (Lit i)     = i
  eval' (Add e0 e1) = go e0 + go e1

memoInterpret :: Expr -> String
memoInterpret = go where
  go = memo eval'
  eval' (Lit i)     = "Lit " ++ show i
  eval' (Add e0 e1) = "Add (" ++ go e0 ++ ") (" ++ go e1 ++ ")"

graphEval :: Expr -> Int
graphEval expr = consume reified where
  reified = unsafePerformIO (toGraph <$> reifyGraph expr)
  toGraph (Reify.Graph env _) = graphFromEdges . map toNode $ env
  toNode (j, AddF a b) = (AddF a b, j, [a, b])
  toNode (j, LitF d)   = (LitF d, j, [])

consume :: Eq a => (Graph, Vertex -> (ExprF a, a, b), c) -> Int
consume (g, vmap, _) = go (reverse . topSort $ g) [] where
  go [] acc = snd $ head acc
  go (v:vs) acc =
    let nacc = evalNode (vmap v) acc : acc
    in  go vs nacc

evalNode :: Eq a => (ExprF a, b, c) -> [(a, Int)] -> (b, Int)
evalNode (LitF d, k, _)   _ = (k, d)
evalNode (AddF a b, k, _) l =
  let v = fromJust ((+) <$> lookup a l <*> lookup b l)
  in  (k, v)

tree :: (Eq a, Num a, Num b) => a -> b
tree 0 = 1
tree n =
  let shared = tree (n - 1)
  in  shared + shared

