#use "topfind"
#camlp4o
#require "camlp4.listcomprehension"

(* 2.1 Arithmetic expressions *)

type 'a expr = Val of 'a 
             | Add of ('a expr)*('a expr)

let f = Add ((Val 1, Add (Val 2,Val 3)))
let g = Add (Add (Val 1, Val 2), Add (Val 3, Val 4))

let rec eval : int expr -> int = fun x ->
  match x with
  | Val n -> n
  | Add (x,y) -> (eval x) + (eval y)

let res = eval f

(* 2.2 Fold for expressions *)

let rec fold f g = function
  | (Val n) -> f n
  | Add (x,y) -> g (fold f g x) (fold f g y)

type inst = PushI of int | AddI

let comp : int expr -> inst list = 
  let f n = [PushI n] in 
  let g xs ys = xs@ys @ [AddI] in 
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


(* 2.3 Generalizing 
 --------------------

   refs:
    * Hutton : A tutorial on universality & expressivity of folds 

 fold f g h (Val n)   = f n
 fold f g h (Add x y) = g (fold f g h x) (fold f g h y)
 fold f g h (Var c)   = h c

 eval :: Expr Int -> (Store -> Int)
 eval = fold f g h
        where
          f n     = \s -> n
          g fx fy = \s -> fx s + fy s
          h c     = \s -> find c s

 eval (Add (Var "x") (Val 5)) = 
 fold f g h (Add (Var "x") (Val 5)) =
 g (fold f g h (Var "x")) (fold f g h (Val 5)) = 
 \s -> (fold f g h (Var "x")) s + (fold f g h (Val 5)) s
 \s -> (h (Var "x")) s + (f 5) s =
 \s -> (\s' -> find (Var "x") s') s + (\s' -> 5) s = 
 \s -> find (Var "x") s + 5

 eval :: Expr Int -> (Store -> Int)

*)

(* 3 Operational semantics 
   refs:
     * Plotkin: Structural approach to operatrional semantics 

*)

(* 3.1 Arithmetic expressions *)

let rec trans : int expr -> int expr list = 
  function
  | Val n -> []
  | Add (Val n, Val m) -> [Val (n+m)]
  | Add (x,y) -> [Add (x',y) | x' <- trans x] @ 
                 [Add (x,y') | y' <- trans y]

type 'a tree = Node of 'a*('a tree list)

let rec exec : int expr -> int expr tree =
  fun e -> Node (e,[exec e' | e' <- trans e])
