-- Type-Safe Observable sharing

-- shallow embedding 

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


parity :: Bit -> Bit
parity input = output
  where 
    output = xor (delay output) input

