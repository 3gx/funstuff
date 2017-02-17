parity :: Bit -> Bit
parity input = output
  where 
    output = xor (delay output) input

-- deep embedding

data Bit = Xor Bit Bit
         | Delay Bit
         | Input [Bool]
         | Var String deriving Show 
xor = Xor
delay = Delay

run :: (Bit -> Bit) -> [Bool] -> [Bool]
run f bs = interp (f (Input bs))

interp :: Bit -> [Bool]
interp (Xor b1 b2) = zipWith (/=) (interp b1) (interp b2)
interp (Delay b) = False : interp b
interp (Input bs) = bs
inputer (Var v) = error $ "Var not supported"

smth :: [Int] -> [Int]
smth xs = 0:xs

binop :: [Int] -> [Int] -> [Int]
binop xs ys = zipWith (+) xs ys

-- > run parity (replicate 10 True)
-- [True,False,True,False,True,False,True,False,True,False]
-- >  parity (Var "x")
-- Xor (Delay (Xor (Delay (Xor (Delay (Xor 





{- digression: recursion -}

f :: [Int] -> [Int]
f x = map (+100) (1:(f x)) 
-- > zip (f [1]) [1..10]
-- [(101,1),(201,2),(301,3),(401,4),(501,5),(601,6),(701,7),(801,8),(901,9),(1001,10)]

{-
   f [1] =
== map (+100) (1:(f [1]))
== 101 : map (+100) (f [1]) <<<-----
== 101 : map(+100) (map (+100) 1:(f [1]))
== 101 : map(+200) (1 : (f [1])) 
== 101 : 201 : map (+200) (f [1])  <<<-------
== 101 : 201 : map (+200) (map (+100) (1:(f [1])))
== 101 : 201 : map (+300) (1:(f [1]))
== 101 : 201 : 301 : map (+300) (f [1])  <<<-------
...

-}

zw :: (Int->Int->Int) -> [Int] -> [Int] -> [Int]
zw f (x:xs) (y:ys) = f x y : zw f xs ys
zw f _ _ = []


g :: [Int] -> [Int]
g x = zw (+) (0:(g x))  x

g' :: [Int] -> [Int]
g' x = zw (+) (0:(g' x))  (0:(init x))

g'' :: [Int] -> [Int]
g'' x = zw (+) (g'' x)  x


-- > g [10..14]
-- [10,21,33,46,60]
--
-- > take 3 $ g [10..14]
-- [10,21,33]
-- > take 3 $ g' [10..14]
-- [Hang] ^C


{-
 
   g [10..14]
== zw (+) (0:(g [10..14]) [10..14]
== 10 : zw (+) (g [10..14]) [11..14]

   zw (+) (g [10..14]) [11..14]
== zw (+) (10 : zw (+) (g [10..14]) [11..14]) [11..14]
== 21 : zw (+) (zw (+) (g [10..14]) [11..14]) [12..14]

   zw (+) (zw (+) (g [10..14]) [11..14]) [12..14]
== zw (+) (21 : zw (+) (zw (+) (g [10..14]) [11..14]) [12..14]) [12..14]
== 33 : zw (+) (zw (+) (zw (+) (g [10..14]) [11..14]) [12..14]) [13,14]

   zw (+) (zw (+) (zw (+) (g [10..14]) [11..14]) [12..14]) [13,14]
== zw (+) (33 : zw (+) (zw (+) (zw (+) (g [10..14]) [11..14]) [12..14])) [13,14]
== 46 : zw (+) (zw (+) (zw (+) (zw (+) (g [10..14]) [11..14]) [12..14])) [14]

== zw (+) (zw (+) (zw (+) (zw (+) (g [10..14]) [11..14]) [12..14])) [14]
== zw (+) (46: zw (+) (zw (+) (zw (+) (zw (+) (g [10..14]) [11..14]) [12..14]))) [14]
== 60 : zw (+) (zw (+) (zw (+) (zw (+) (zw (+) (g [10..14]) [11..14]) [12..14]))) []
== 60


g [10..14] = 10:21:33:46:60[]
  
   g'' [10..14]
== zw (+) (g'' [10..14]) [10..14]
== zw (+) (zw' (+) (g'' [10..14]) [10..14]) [10..14]
== zw (+) (zw (+) (zw (+) (g'' [10..14]) [10..14]) [10..14]) [10..14]
== [Hang]

 -}

