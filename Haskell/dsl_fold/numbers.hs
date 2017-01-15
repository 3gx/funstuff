data Number = Zero | Succ Number deriving Show

one :: Number
one = Succ Zero

two :: Number
two = Succ one

plus' :: Number -> Number -> Number
plus'  x Zero = x
--plus' (Succ x) y = Succ (plus' x y)
plus' x (Succ y) = Succ (plus' x y)

three :: Number
three = plus' two one

max' :: Number -> Number -> Number
max' Zero y = y
max' x Zero = x
max' (Succ x) (Succ y) = Succ (max' x y)

min' :: Number -> Number -> Number
min' (Succ x) (Succ y) = Succ (min' x y) 
min' _ _ = Zero
