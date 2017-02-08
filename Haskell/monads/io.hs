import Data.Char (chr)

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


