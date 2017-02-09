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





