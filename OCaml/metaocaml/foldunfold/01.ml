(* 2.1 Arithmetic expressions *)

type 'a expr = Val of 'a 
             | Add of ('a expr)*('a expr)

let f = Add ((Val 1, Add (Val 2,Val 3)))

let rec eval : int expr -> int = fun x ->
  match x with
  | Val n -> n
  | Add (x,y) -> (eval x) + (eval y)

let res = eval f

(* 2.2 Fold for expressions *)

let rec fold f g = function
  | (Val n) -> f n
  | Add (x,y) -> g (fold f g x) (fold f g y)

type inst = Push of int | Add

let comp : int expr -> inst list = 
  let f n = [Push n] in 
  let g xs ys = xs@ys @ [Add] in 
  fold f g

let stmts = comp f

let id x = x

let eval1 = fold id (+)

let res1 = eval1 f

(*
   f : a -> b      g : b -> b -> b
  -----------------------------------
        fold f g : 'a expr -> b
 *)


(* 2.3 Generalizing *)


