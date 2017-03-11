module DataBase where


-- Data variants for literals and addition

data Lit = Lit Int
data (Exp l, Exp r) => Add l r = Add l r


-- The open union of data variants

class Exp x
instance Exp Lit
instance (Exp l, Exp r) => Exp (Add l r)
