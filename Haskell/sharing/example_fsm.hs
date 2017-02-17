{-# LANGUAGE TypeFamilies #-}

-- 5. Example : Finite State Machine


import Data.Reify

{-
data State = ZeroZero | ZeroOne | OneZero | OneOne deriving Show
type Input = Bool
type Output = (Bool,Bool)

step :: Input -> State -> (Output, State)
step False ZeroZero = ((False,False), ZeroZero)
step True  ZeroZero = ((True ,True ), ZeroOne)
step False ZeroOne  = ((True ,True), OneOne)
step True  ZeroOne  = ((False,False), OneZero)
step False OneZero  = ((False,True ), ZeroZero)
step True  OneZero  = ((True ,False), ZeroOne)
step False OneOne   = ((True ,False), OneZero)
step True  OneOne   = ((False,True) , OneOne)
-}

data State i o = State [(i,(o,State i o))] deriving Show

step :: (Eq i) => i -> State i o -> (o, State i o)
step i (State ts) = (output, st)
  where 
    Just (output,st) = lookup i ts

state00 = State [ (False,((False,False), state01)),
                  (True, ((True ,True),  state00))]
state01 = State [ (False,((True ,True ), state11)),
                  (True, ((False,False), state10))]
state10 = State [ (False,((False,True),  state00)),
                  (True, ((True ,False), state01))]
state11 = State [ (False,((True ,False), state10)),
                  (True, ((False,True),  state11))]

run :: (Eq i) => State i o -> [i] -> [o]
run st (i:is) = o : run st' is
  where
    (o,st') = step i st

data StateNode i o s = StateNode [ (i, (o,s) )] deriving Show

instance MuRef (State i o) where
  type DeRef (State i o) = StateNode i o
  mapDeRef f (State st) = StateNode <$> traverse tState st
       where
    tState (b,(o,s)) = (\s' -> (b,(o,s')))  <$>  f s
                          
