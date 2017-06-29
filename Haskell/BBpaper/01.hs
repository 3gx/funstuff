{-# LANGUAGE GADTs #-}

import Control.Monad

{- API:
   move      :: Program ()
   turnLeft  :: Program ()
   turnRight :: Program ()
   sensor    :: Progrma Bool
   cond      :: Program Bool -> Program () -> Program () -> Program ()
   while     :: Program Bool -> Program () -> Program () -

   sMove :: Program ()
   sMove = cond sensor turnRigth move

   moveToWall :: Program ()
   moveToWall = while ((liftM not) sensor) move
-}



type Name = String

data BoolE = Lit Bool
           | Var Name
           | (:||:) BoolE BoolE
           | (:&&:) BoolE BoolE
           | Not BoolE

data Program a where
  Move      :: Program ()
  TurnLeft  :: Program ()
  TurnRight :: Program ()
  Sensor    :: Program BoolE
  Cond      :: Program BoolE -> Program () -> Program ()
  While     :: Program BoolE -> Program () -> Program ()
  Return    :: a -> Program a
  Bind      :: Program a -> (a -> Program b) -> Program b

instance Monad Program where
  return = Return
  (>>=)  = Bind


instance Applicative Program  where
  pure = Return
  (<*>)  = undefined

instance Functor Program  where
  fmap = undefined


            

