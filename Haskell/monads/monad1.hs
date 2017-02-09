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

-- > addStuff 3
-- 19
-- > addStuff1 3
-- 19
-- 