import System.Random

type Stack = [Int]

pop' :: Stack -> (Int, Stack)
pop' (x:xs) = (x,xs)

push' :: Int -> Stack -> ((),Stack)
push' a xs = ((),a:xs)

stackManip' :: Stack -> (Int, Stack)
stackManip' stack = let
    ((),newStack1) = push' 3 stack
    (a ,newStack2) = pop' newStack1
    in pop' newStack2

-- > stackManip' [5,8,2,1]
-- (5,[8,2,1])


-- state Monad

newtype State s a = State { runState :: s -> (a,s) }

instance Functor (State s) where
  fmap f (State mx) = State (\s -> let (x,s') = mx s
                                   in (f x, s'))

instance Applicative (State s) where
  pure x = State $ \s -> (x,s)
  (State mf) <*> (State mx) = State $ (\s -> let (f,s') = mf s
                                                 (x,s'') = mx s'
                                             in (f x, s''))
instance Monad (State s) where
  return x = State $ \s -> (x,s)
  (State h) >>= f = State $ \s -> let (x,s') = h s
                                      (State y) = f x
                                  in y s' 

pop :: State Stack Int
pop = State $ \(x:xs) -> (x,xs)

push :: Int -> State Stack ()
push a = State $ \xs -> ((), a:xs)

stackManip :: State Stack Int
stackManip = do
  push 3
  pop  
  pop

--  runState stackManip [5,8,2,1]
--  (5,[8,2,1])


stackStuff :: State Stack ()
stackStuff = do
  a <- pop
  if a == 5
    then push 5
    else do
      push 3
      push 8

-- > runState  stackStuff [5,8,2,1]
-- ((),[5,8,2,1])
-- > runState  stackStuff [1,5,8,2,1]
-- ((),[8,3,5,8,2,1])


get :: State Stack Stack
get = State $ \s -> (s,s)

put :: Stack -> State Stack ()
put s = State $ \_ -> ((),s)

stackyStack :: State Stack ()
stackyStack = do
  stackNow <- get
  if stackNow == [1,2,3]
    then put [8,3,1]
    else put [9,2,1]

-- > runState stackyStack [1,2,3]
-- ((),[8,3,1])
-- > runState stackyStack [1,2,4]
-- ((),[9,2,1])


randomSt :: (RandomGen g, Random a) => State g a  
randomSt = State random  

threeCoins :: State StdGen (Bool,Bool,Bool)  
threeCoins = do  
    a <- randomSt  
    b <- randomSt  
    c <- randomSt  
    return (a,b,c) 

-- > runState threeCoins (mkStdGen 33)  
-- ((True,False,True),680029187 2103410263u
-- > runState threeCoins (snd $ runState threeCoins (mkStdGen 33)  )
-- ((True,True,True),1470354523 1780294415)

type Seed = Int
drand48val :: Seed -> Double
drand48val s = realToFrac(s `mod` 281474976710656)*(1.0/281474976710656.0);

nextSeed :: Seed -> Seed
nextSeed s = s * 0x5DEECE66D + 0xB;

type DRand48 a = State Seed a

getNextRand48 :: DRand48 Double
getNextRand48 = State (\s -> (drand48val(s), nextSeed(s)))

tf :: Double -> Bool
tf x | x > 0.5 = True| otherwise = False

threeCoins' :: DRand48 (Bool,Bool,Bool)
threeCoins' = do 
  a <- getNextRand48
  b <- getNextRand48
  c <- getNextRand48
  return (tf a, tf b, tf c)

-- > runState threeCoins' (-5753696520228384159)
-- ((True,False,False),-6755292999987930158)
-- > runState threeCoins' (5753696520228384159)
-- ((False,True,True),-6913108432911116632)



