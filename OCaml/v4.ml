type colour = Red 
            | Green 
            | Blue 
            | Yellow
            | RGB of int * int * int

type 'a option = None | Some of 'a

let cols = [Red; Red; Green; Yellow; RGB (150,0,255)]

let nothing = None
let number = Some 50
let numbers = [Some 12; None; None; Some 2]
let word = Some ['c'; 'a'; 'k'; 'e']

type 'a sequence = Nil | Cons of 'a * 'a sequence

let llist = Cons ('a', Cons ('x', Cons ('e', Nil)))

type expr = 
    Num of int
  | Add of expr * expr
  | Sub of expr * expr
  | Mul of expr * expr
  | Div of expr * expr

let expr1 = Add (Num 1, Mul (Num 2, Num 3))

let rec evaluate e = 
  match e with 
    Num x -> x
  | Add (e, e') -> evaluate e + evaluate e'
  | Sub (e, e') -> evaluate e - evaluate e'
  | Mul (e, e') -> evaluate e * evaluate e'
  | Div (e, e') -> evaluate e / evaluate e'
  

