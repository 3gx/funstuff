
-- See: https://wiki.haskell.org/IO_inside

import Data.Char (chr,toUpper)
import Data.IORef
import Data.Array.IO

-- simple sequences execution examples

getchar :: Int -> (Char,Int)
getchar n = (chr (33+n),n+1)

get2chars :: Int -> (String,Int)
get2chars i0 = ([a,b],i2) where (a,i1) = getchar i0
                                (b,i2) = getchar i1

get4chars :: Int -> (String, Int)
get4chars i0 = (a++b, i2) where (a,i1) = get2chars i0
                                (b,i2) = get2chars i1

-- > getchars 40
-- ("IJKL",44)

-----

mainx world0 = 
            let (a,world1) = getchar world0
                (b,world2) = getchar world1
            in ((), world2)

ask :: String -> IO Int
ask s = putStr s >> readLn;

 


when :: Bool -> IO () -> IO () -> IO ()
when condition action world = 
  if condition 
    then action
    else world

(action1 .|>. action2) world0 =
   let (a, world1) = action1 world0
       (b, world2) = action2 world1
   in (b, world2)


action1 .>>. action2 = action
  where
    action world0 = let (a, world1) = action1 world0
                        (b, world2) = action2 world1
                    in (b, world2)

liftm :: (a->b) -> IO a -> IO b
liftm f action = action >>= \x -> return (f x)


touppers :: String -> String
touppers (x:xs) | null xs = [toUpper x] | otherwise = toUpper x :touppers xs

touppersm = liftm touppers

while :: IO Bool -> IO ()
while mval = do
  val <- mval
  if (val) then  do
    c <- getChar
    if (c == ' ') then while (return False) else while (return True)
  else do
    return ()

-- mutable data

main = do  
     --- Mutable data
      arr <- newArray (1,10) 37 :: IO (IOArray Int Int)
      a <- readArray arr 1
      writeArray arr 1 64
      b <- readArray arr 1
      print (a,b)
       ---------
      varA <- newIORef 0
      a0 <- readIORef varA
      writeIORef varA 1
      a1 <- readIORef varA
      print (a0,a1) 
     ---
      str <- (touppersm.return) "wow!"
      print str
     ---
      let get2chars = getChar >> getChar
      putStr "Press two keys"
      get2chars
      a <- ask "Whats1?"
      b <- ask "Whats2?"
      return ()
