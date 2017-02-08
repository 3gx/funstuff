import Data.Char (chr,toUpper)

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

main = do  
      str <- (touppersm.return) "wow!"
      print str
      a <- ask "Whats1?"
      b <- ask "Whats2?"
      return ();
