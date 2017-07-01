type Id = String

data Type = Vec Int 
          | Matrix Int Int 
          | Output
    deriving Eq

data DExp = 
    LitInt Int | LitFloat Float | LitBool Bool
  | Var Id Type | Led Id Type DExp | If DExp DExp DExp
  | Add DExp DExp |  Mul DExp DExp | Sub DExp DExp | Div DExp DExp
  | Sin DExp | Cos DExp | Sqrt DExp
  | Or DExp DExp | And DExp DExp | Not DExp
