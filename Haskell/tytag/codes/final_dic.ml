(* Tagless Final using dictionary passing *)

(* We use objects as extensible records, to model
   the implicit dictionary composition in Haskell
*)

(* Compare with Haskell's ExpSYM *)
class type ['repr] expSYM  = object
  method lit : int -> 'repr
  method neg : 'repr -> 'repr
  method add : 'repr -> 'repr -> 'repr
end;;

(* Constructor functions *)
let lit n = fun ro -> ro#lit n;;
let neg e = fun ro -> ro#neg (e ro);;
let add e1 e2 = fun ro -> ro#add (e1 ro) (e2 ro);;

(* Unit is for the sake of value restriction *)
(* The term is exactly the same as that in Intro2.hs *)
let tf1 () = add (lit 8) (neg (add (lit 1) (lit 2)));;

(* We can write interepreters of expSYM *)
(* and evaluate exp in several ways. The code for the interpreters
   is quite like the one we have seen already
*)
class eval = object 
  method lit n = (n:int)
  method neg e = - e
  method add e1 e2 = e1 + e2
end;;

let eval = new eval;;

(* We didn't apply eval, we pass eval as an argument *)
let 5  = tf1 () eval;;

class view = object
  method lit n = string_of_int n
  method neg e = "(-" ^ e ^ ")"
  method add e1 e2 = "(" ^ e1 ^ " + " ^ e2 ^ ")"
end;;

let view = new view;;

let "(8 + (-(1 + 2)))" = tf1 () view;;

(* We can extend our expression adding a new expression form *)
class type ['repr] mulSYM = object
  method mul : 'repr -> 'repr -> 'repr
end;;

let mul e1 e2 = fun ro -> ro#mul (e1 ro) (e2 ro);;


(* Extended sample expressions *)
(* Again, the code is the same as before, modulo the occasional () *)
(* Value restriction is indeed annoying ... *)
let tfm1 () = add (lit 7) (neg (mul (lit 1) (lit 2)));;

let tfm2 () = mul (lit 7) (tf1 ());;

class evalM = object 
  inherit eval
  method mul e1 e2 = e1 * e2
end;;

let evalM = new evalM;;

class viewM = object
  inherit view
  method mul e1 e2 = "(" ^ e1 ^ " * " ^ e2 ^ ")"
end;;

let viewM = new viewM;;

(* can use the extended evaluator to evaluate old expressions *)
let 5  = tf1 () evalM;;

(* Of course we can't use the old evaluator to evaluate extended
   expressions
let 5 = tfm1 () eval;;
Error: This expression has type eval but an expression was expected of type
         < add : 'a -> 'b -> 'c; lit : int -> 'a; mul : 'a -> 'a -> 'd;
           neg : 'd -> 'b; .. >
       The first object type has no method mul
*)

let 5 = tfm1 () evalM;;

let 35 = tfm2 () evalM;;

let "(7 + (-(1 * 2)))" = tfm1 () viewM;;

let "(7 * (8 + (-(1 + 2))))" = tfm2 () viewM;;

(* The expressions are first-class: we can put them into the same list *)

let tl1 () = [lit 1; tf1 ()];;

(* and add the extended objects afterwards *)

let tl2 () = tfm1 () :: tfm2 () :: tl1 ();;

let [5; 35; 1; 5] = List.map (fun x -> x evalM) (tl2 ());;

let ["(7 + (-(1 * 2)))"; "(7 * (8 + (-(1 + 2))))"; "1"; "(8 + (-(1 + 2)))"]
    = List.map (fun x -> x viewM) (tl2 ());;

Printf.printf "\nAll done\n";;
