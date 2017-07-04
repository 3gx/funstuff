{-# LANGUAGE GADTs #-}

import Control.Monad

{- API:
   ----
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
  Cond      :: Program BoolE -> Program () -> Program () -> Program ()
  While     :: Program BoolE -> Program () -> Program ()
  Return    :: a -> Program a
  Bind      :: Program a -> (a -> Program b) -> Program b

instance Monad Program where
  return = Return
  (>>=)  = Bind


instance Applicative Program  where
  pure = return
  mf <*> mx = do
    f <- mf
    x <- mx
    return $ f x

instance Functor Program  where
  fmap f mx = do
    x <- mx
    return $ f x

move = undefined
turnLeft = undefined

spiralIn :: Int -> Program ()
spiralIn 0 = return ()
spiralIn n = do
  replicateM_ 2 $ do
    replicateM_ n move
    turnLeft
  spiralIn (n-1)
            

data Prg = PMove
         | PTurnRight
         | PTurnLeft
         | PSensor Name
         | PCond BoolE Prg Prg
         | PWhile Name Prg Prg

         -- PSeq replaced bind
         | PSeq Prg Prg
         | PSkip
         
         -- PAssign is used by While in Program
         | PAssign Name BoolE

newNameSupply = undefined
supplyValue x = 42

split :: a->(a,a,a)
split = undefined

runCompile :: Program a -> Prg
runCompile prg = snd $ compile s prg
  where s = newNameSupply

-- compile :: NAmeSupply -> Program a -> (a, Prg)
compile :: a -> Program a -> (a, Prg)
compile s Move = ((), PMove)
compile s TurnRight = ((), PTurnRight)
compile s TurnLeft = ((), PTurnLeft)

compile s Sensor = (Var nom, PSensor nom)
  where
      v = supplyValue s
      nom = "v" ++ show v

compiler s (Cond b p1 p2) = ((), bp `PSeq` PCond b' p1' p2')
  where
    (s1,s2,s3) = split3 s
    (b', bp) = compile s1 b
    (a1,p1') = compile s2 p1
    (a2,p2') = compile s3 p2

