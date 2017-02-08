newtype State s a = State { runState :: s -> (a,s) };

returnState :: a -> State s a
returnState a = State $ \s -> (a,s)

bindState :: State s a -> (a -> State s b) -> State s b
bindState m k = State $ \s -> let (a, s') = runState m s
                              in runState (k a) s'

updateState :: Int -> Int
updateState stat = stat * 0x5DEECE66D + 0xB;

drand48 :: Int -> Double
drand48 state = realToFrac(state `mod` 281474976710656)*(1.0/281474976710656.0);



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
