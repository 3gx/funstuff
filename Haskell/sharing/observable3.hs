-- http://stackoverflow.com/questions/9732084/how-do-you-represent-a-graph-in-haskell

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
import Data.Reify
import Control.Applicative
import Data.Traversable

--Pointer-based graph representation
data PtrNode = PtrNode [PtrNode] deriving Show

--Label-based graph representation
data LblNode lbl = LblNode [lbl] deriving Show

--Convenience functions for our DSL
leaf      = PtrNode []
node1 a   = PtrNode [a]
node2 a b = PtrNode [a, b]


-- This looks scary but we're just telling data-reify where the pointers are
-- in our graph representation so they can be turned to labels
instance MuRef PtrNode where
    type DeRef PtrNode = LblNode
    mapDeRef f (PtrNode as) = LblNode <$> (traverse f as)

-- Graph we want to represent:
--    .----> a <----.
--   /               \
--  b <------------.  \
--   \              \ / 
--    `----> c ----> d

-- Code for the graph:
a = leaf
b = node2 a c
c = node1 d
d = node2 a b
-- Yes, it's that simple!


-- If you want to convert the graph to a Node-Label format:
main = do
    g <- reifyGraph b   --can't use 'a' because not all nodes are reachable
    print g
