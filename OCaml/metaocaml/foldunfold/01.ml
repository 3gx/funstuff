type 'a expr = Val of 'a 
             | Add of ('a expr)*('a expr)

let f = Add ((Val 1, Add (Val 2,Val 3)))

let rec eval : int expr -> int = fun x ->
  match x with
  | Val n -> n
  | Add (x,y) -> (eval x) + (eval y)

let res = eval f
