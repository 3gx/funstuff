factorial = \n -> if (n == 0) then 1 else n * factorial (n-1)

--sort_of_factorial = \n -> if (n == 0) then 1 else n * xxx (n-1)

almost_factorial = \f -> \n -> if (n == 0) then 1 else n * f (n-1)

-- factorial' = ycomb almost_factorial
-- factorialB = almost-factorial factorialA
-- factorialB = \n -> if (n == 0) then 1 else n * factorialA (n-1)
--
fac' = almost_factorial.fac'

identity = \x -> x
fac0 = almost_factorial identity
fac1 = almost_factorial fac0
fac1' = almost_factorial $ almost_factorial identity
fac99 = almost_factorial $ almost_factorial $ almost_factorial $ 
        almost_factorial $ almost_factorial $ almost_factorial $  
        almost_factorial $ almost_factorial $ almost_factorial $  
        almost_factorial $ almost_factorial $ almost_factorial identity


ycomb :: (t -> t) -> t
ycomb = \f -> f (ycomb f)

ycomb' = \f -> f $ \x -> (ycomb' f) x

factorial' = ycomb almost_factorial
factorial'' = ycomb' almost_factorial

fix = \f -> f (fix f)
fact = fix almost_factorial
---

--part_factorial :: (Integer->Integer)->Integer->Integer
--part_factorial  self n= if (n == 0) then 1 else n * (self (n-1))
--part_factorial  self = \n -> if (n == 0) then 1 else n * ((self self) (n-1))
--

--ycomb1 = \x -> f (\y -> (x x) y)

main = do
  print $ factorial 3
  print $ factorial 6
  print $ factorial 9
  print $ fac' (\_->42) 9
  print $ factorial' 3
  print $ factorial' 6
  print $ factorial' 9
