{-# LANGUAGE DeriveFunctor #-}
--{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}

import Data.Reify

-- type Unique = Int

data BitNode s = GraphXor s s 
               | GraphDelay s
               | GraphInput [Bool]
               | GraphVar String 
          deriving (Show,Functor)

data BitGraph = BitGraph [(Unique,BitNode Unique)] Unique deriving Show

graph = BitGraph [ (1, GraphXor 2 3)
                 , (2, GraphDelay 1)
                 , (3, GraphVar "x")
                 ] 1

data Bit = Xor Bit Bit
         | Delay Bit
         | Input [Bool]
         | Var String deriving Show 

parity :: Bit -> Bit
parity input = output
  where 
    output = xor (delay output) input

xor = Xor
delay = Delay

-- reifyBitGraph :: Bit -> IO BitGraph

-- data Graph e = Graph [(Unique, e Unique)] Unique

-- type BitGraph' = Graph BitNode 

-- requires FlexibleInstances [todo: figure out why]
--instance Show (Graph BitNode) where
--  show (Graph s u) = "Graph BitNode " ++ show s ++ " " ++ show u


graph' = Graph [ (1, GraphXor 2 3)
               , (2, GraphDelay 1)
               , (3, GraphVar "x")
               ] 1

--class MuRef a where 
 -- type DeRef a :: * -> *
  --mapDeRef      :: (Applicative f) 
   --             => (a -> f u)
    --            -> a
     --           -> f (DeRef a u)
               
instance MuRef Bit where
  type DeRef Bit = BitNode
  mapDeRef f (Xor a b)  = GraphXor <$> f a <*> f b
  mapDeRef f (Delay b)  = GraphDelay <$> f b
  mapDeRef f (Input bs) = pure $ GraphInput bs
  mapDeRef f (Var nm)   = pure $ GraphVar nm

-- >reifyGraph $ parity (Var "x")
-- let [(1,GraphXor 2 3),(3,GraphVar "x"),(2,GraphDelay 1)] in 1


