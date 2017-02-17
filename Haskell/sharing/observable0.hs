-- Type-Safe Observable sharing

{- shallow embedding 

newtype Bit = Bit [Bool] deriving Show

xor :: Bit -> Bit -> Bit
xor (Bit xs) (Bit ys) = Bit $ zipWith (/=) xs ys

delay :: Bit -> Bit
delay (Bit xs) = Bit $ False : xs


run :: (Bit -> Bit) -> [Bool] -> [Bool]
run f bs = rs
  where
    (Bit rs) = f (Bit bs)

-- > run parity [True, False,True,False]
-- [True,True,False,False]

-}

parity :: Bit -> Bit
parity input = output
  where 
    output = xor (delay output) input

-- deep embedding

data Bit = Xor Bit Bit
         | Delay Bit
         | Input [Bool]
         | Var String
       deriving Show

xor = Xor
delay = Delay

run :: (Bit -> Bit) -> [Bool] -> [Bool]
run f bs = interp (f (Input bs))

interp :: Bit -> [Bool]
interp (Xor b1 b2) = zipWith (/=) (interp b1) (interp b2)
interp (Delay b) = False : interp b
interp (Input bs) = bs
inputer (Var v) = error $ "Var not supported"
