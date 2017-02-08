--{-# LANGUAGE GeneralizedNewtypeDeriving #-}

newtype State s a = State { runState :: s -> (a,s) };

instance Functor (State s) where
  fmap f (State s) = State (\s' -> let (x,s'') = s s'
                                   in (f x, s''))

instance Applicative (State s) where
  pure x = State $ \s -> (x,s)
  (State mf) <*> (State mx) = State (\s -> let (f,s')  = mf s
                                               (x,s'') = mx s'
                                           in (f x, s''))

instance Monad (State s) where
  return x = State $ \s -> (x,s)
  (State mx) >>= f = State (\s -> let (x, s') = mx s
                                  in runState (f x) s')

get :: State s s 
get = State $ \s -> (s,s)

put :: s -> State s ()
put s = State $ \_ -> ((),s)

modify :: (s -> s) -> State s ()
modify f = do { x <- get; put (f x); }

gets :: (s -> a) -> State s a
gets f = get >>= \x -> return (f x);
-- gets f = do { x<-get; return (f x); }

{-
     get >>= \x -> return (f x)

 ==  State (\s -> (s,s)) >>= (\x -> return (f x))
 
==  State (\s -> (s,s)) >>= (\x -> State (\s -> (f x, s)))

 ==  (>>=) State (\s -> (s,s)) (\x -> (State (\s -> (f x,s))) )

 ==  State (\s -> let (x,s') = (\s -> (s,s)) s
                  in runState (\x -> (State (\s -> (f x,s)))) x  s')

 == State (\s -> let (x,s') = (s,s)  
                 in runState (State \s -> (f x, s)) s')

 == State (\s -> let (x,s') = (s,s)
                 in (\s -> (f x, s)) s')

 == State (\s -> let (x,s') = (s,s)
                 in (f x, s'))

 == State (\s -> (f s , s))

 -}

evalState :: State s a -> s -> a
evalState act = fst . runState act

execState :: State s a -> s -> s
execState act = snd . runState act

data State' s a = State' (s->a) ( s->s)
runState' (State' f tr) s = (f s, tr s)

instance Functor (State' s) where
  fmap f (State' g tr) = State' (f.g) tr

instance Applicative (State' s) where
  pure x = State' (const x) id
  State' fg tr1 <*> State' fx tr2 = State' ff (tr1 . tr2)
    where
      ff st = let g = fg st
                  x = fx (tr1 st)
              in g x

instance Monad (State' s) where
  return x = State' (const x) id
  State' fx tr >>= k = State' ff ttr
    where
      ff st  = let x   = fx st
                   st' = tr st
                   State' fy tr' = k x
               in fy st'
      ttr st = let x   = fx st
                   st' = tr st
                   State' fy tr' = k x
               in tr' st'

join :: State' s (State' s a) -> State' s a
join (State' ff ttr) = State' f' tr'
  where
    f' st  = let State' f tr = ff st
                 st'         = ttr st
             in f st'
    tr' st = let State' f tr = ff st
                 st'         = ttr st
             in tr st'

     
  

returnState :: a -> State s a
returnState a = State $ \s -> (a,s)

bindState :: State s a -> (a -> State s b) -> State s b
bindState m k = State $ \s -> let (a, s') = runState m s
                              in runState (k a) s'

---------- drand48 ()

type Seed = Int
drand48val :: Seed -> Double
drand48val s = realToFrac(s `mod` 281474976710656)*(1.0/281474976710656.0);

nextSeed :: Seed -> Seed
nextSeed s = s * 0x5DEECE66D + 0xB;

type DRand48 = State Seed Double

getNextRand48 :: DRand48
getNextRand48 = State (\s -> (drand48val(s), nextSeed(s)))

rand48 :: Int -> DRand48
rand48 count | count == 1 = getNextRand48
             | otherwise =  getNextRand48 >> rand48 (count-1)
  
 
getRand :: Seed -> Int -> Double
getRand  seed count = evalState (rand48 count) seed


{-
 
  struct Rand48
  {
    double drand()
    {
      update();
      return (stat&0xFFFFFFFFFFFF)*(1.0/281474976710656.0);
    }
    long lrand()
    {
      update();
      return (long)(stat>>17)&0x7FFFFFFF;
    }
    long mrand()
    {
      update();
      return(long)(stat>>16)&0xFFFFFFFF;
    }
    void srand(const long seed)
    {
      stat = (seed<<16)+0x330E;
    }

    private:
    long long stat;
    void update()
    {
      stat = stat*0x5DEECE66D + 0xB;
    }
  };

-}


---------

type GameValue = Int
type GameState = (Bool, Int)

playGame :: String  -> State GameState GameValue
playGame [] = do
  (_, score) <- get
  return score

playGame (x:xs) = do
  (on, score) <- get
  case x of
    'a' | on -> put (on,score+1)
    'b' | on -> put (on,score-1)
    'c'      -> put (not on, score)
    _        -> put (on,score)
  playGame xs

startState = (False,0::Int)

--------


type MyState = Int

valFromState :: MyState -> Int
valFromState s = -s
nextState ::  MyState -> MyState
nextState x = 1+x

type MyStateMonad = State MyState


getNext :: MyStateMonad Int
getNext = State (\st -> let st' = nextState(st) 
                        in (valFromState(st'), st') )

inc3  :: MyStateMonad Int
inc3 = getNext >>= \x ->
       getNext >>= \y ->
       getNext >>= \z ->
       return z

inc3AlternateResult :: MyStateMonad Int
inc3AlternateResult = do
      getNext
      getNext
      getNext
      s <- get
      return (s*s)

inc4 :: MyStateMonad Int
inc4 = do
        inc3AlternateResult
        getNext

