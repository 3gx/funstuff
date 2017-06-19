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
  
type 'a tree = Br of 'a * 'a tree * 'a tree | Lf

let tree1 = Br (1, Lf, Lf)
let tree2 = Br (2, Br (1, Lf, Lf), Lf)
let tree3 = Br (2, Br(1,Lf,Lf), Br(4, Lf,Lf))

let rec size tr = 
   match tr with 
     Br(_, l, r) -> 1 + size l + size r
  | Lf -> 0

let rec sumtotal tr = 
   match tr with
     Br (x,l,r) -> x + sumtotal l + sumtotal r
   | Lf -> 0

(*val max1 : 'a -> 'a -> 'a *)

let max x y = if x > y then x else y

let rec maxdepth tr = 
  match tr with
    Br (_,l,r) -> 1 + max (maxdepth l) (maxdepth r)
  | Lf -> 0

let rec tree_map f tr = 
  match tr with 
    Br (x, l, r) -> Br (f x, tree_map f l, tree_map f r)
  | Lf -> Lf

let print_dict_entry (k,v) = 
   print_int k ; print_newline (); print_string v; print_newline ()

let rec print_dict d = 
  match d with
   [] -> ()
  | h::t -> print_dict_entry h; print_dict t

let dict1 = [(1,"one"); (2,"two"); (3,"three")]

let rec read_dict () = 
  let i = read_int() in
  if i == 0 then [] else
    let name = read_line() in 
    (i,name) :: read_dict()


let swap a b = 
  let t = !a in
    a := !b; b := t

