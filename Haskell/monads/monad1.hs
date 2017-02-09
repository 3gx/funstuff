addStuff :: Int -> Int  
addStuff = do  
    a <- (*2)  
    b <- (+10)  
    return (a+b)  

addStuff1 = (*2) >>= \a -> (+10) >>= \b -> return (a+b)

{-
    (*2) >>= \a -> (+10) >>= \b -> return (a+b)

 == \w -> (\a -> (+10) >>= \b -> return (a+b)) (w*2) w 

 == \w -> ((+10) >>= \b -> return (w*2+b)) w

 == \w -> (\w' -> (\b->return (w*2+b)) (w'+10) w') w

 == \w -> (\w' -> (return (w*2 +(w'+10))) w') w

 == \w -> (\w' -> (\_ -> (w*2+(w'+10))) w') w

 == \w -> (\w' -> (w*2+ (w'+10)) ) w 

 == \w -> (w*2 + (w+10))

-}

{-
   a <- (*2)
   return (1+a)

   (*2) >>= \a -> return (1+a)

== (*2) >>= \a -> (\_ -> (1+a))

== \w -> (\a -> (\_ -> (1+a))) (w*2) w

== \w -> (\_ -> (1+w*2)) w

== \w -> 1 + 2*w

-}

-- > addStuff 3
-- 19
-- > addStuff1 3
-- 19


{-
   h >>= f = \w -> f (h w) w

   f = (\a -> (+10) >>= \b -> return (a+b))

   f (h w) = (+10) >>= \b -> return ((h w) +b)
 
   (\w' -> (\b->return ((h w) + b)) (h' w') ) w
   (\w' -> return ((h w) + (h' w')) w' ) w
   return ((h w) + (h' w)) w  = (h w) + (h' w)

-}
