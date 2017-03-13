{-# LANGUAGE TypeFamilies #-}

import Data.Map

class Graph g where
  type Vertex g
  data Edge g
  src, tgt :: Edge g -> Vertex g
  outEdges :: g -> Vertex g -> [Edge g]

newtype G1 = G1 [Edge G1]
instance Graph G1 where
  type Vertex G1 = Int
  data Edge   G1 = MkEdge1 (Vertex G1) (Vertex G1)
  src = undefined
  tgt = undefined
  outEdges = undefined

newtype G2 = G2 (Map (Vertex G2) [Vertex G2])
instance Graph G2 where
  type Vertex G2 = Int  
  data Edge   G2 = MkEdge2 Int (Vertex G2) (Vertex G2)
  src = undefined
  tgt = undefined
  outEdges = undefined


{-
class Graph1 g where
  type Vertex1 g
  type Edge1 g
  src1 :: Edge1 g -> Vertex1 g
  outEdges1 :: g -> Vertex1 g -> [Edge1 g]
-}
{-
 [1 of 1] Compiling Main             ( graph.hs, interpreted )

graph.hs:31:3: error:
    • Couldn't match type ‘Vertex1 g0’ with ‘Vertex1 g’
      Expected type: Edge1 g -> Vertex1 g
        Actual type: Edge1 g0 -> Vertex1 g0
      NB: ‘Vertex1’ is a type function, and may not be injective
      The type variable ‘g0’ is ambiguous
    • In the ambiguity check for ‘src1’
      To defer the ambiguity check to use sites, enable AllowAmbiguousTypes
      When checking the class method:
        src1 :: forall g. Graph1 g => Edge1 g -> Vertex1 g
      In the class declaration for ‘Graph1’
Failed, modules loaded: none.

 -}

{-
newtype G3 = G3 Int
instance Graph1 G3 where
  type Vertex1 G3 = Int
  type Edge1 G3 = (Int, Int)
  src1 = undefined
  tgt1 = undefined
  outEdges1 = undefined
-}
