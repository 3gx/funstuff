(* Not quite extensible OCaml objects *)

(* Essentially abstract class, or interface *)
class virtual exp = object
    method virtual eval : int
end;;

(* `constructor objects' *)

class lit n = object
  inherit exp
  method eval = n
end;;

class neg e = object
  inherit exp
  method eval = - e#eval
end;;

class add e1 e2 = object
  inherit exp
  method eval = e1#eval + e2#eval
end;;

(* A sample expression: like in Haskell, with new *)
let tf1 = new add (new lit 8) (new neg (new add (new lit 1) (new lit 2)));;

let 5 = tf1#eval;;



(* We can easily add another variant: multiplication *)

class mul e1 e2 = object
  inherit exp
  method eval = e1#eval * e2#eval
end;;

(* Extended sample expressions *)
let tfm1 = new add (new lit 7) (new neg (new mul (new lit 1) (new lit 2)));;

let tfm2 = new mul (new lit 7) tf1;;

let 5 = tfm1#eval;;

let 35 = tfm2#eval;;

(* The expressions are first-class: we can put them into the same list *)

let tl1 = [new lit 1; tf1];;

(* and add the extended objects afterwards *)

let tl2 = tfm1 :: tfm2 :: tl1;;

let [5; 35; 1; 5] = List.map (fun x -> x#eval) tl2;;





(* However, if we want to add another interpreter -- for example, view,
   we have to change the class 'exp' to add a new method and change
   all children classes lit, neg, add and mul to add a new method view.
   All the code that used the old definition has to be revised and 
   at the very least recompiled. 
   This is the usual story with objects: they permit extensibility in one
   dimension (adding new variants) but not in the other (adding new operations).
*)


