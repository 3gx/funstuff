(* Tagless Final using modules *)

(* Note the similarity with the Haskell type class ExpSYM *)

module type ExpSYM = sig
  type repr
  val lit : int -> repr
  val neg : repr -> repr
  val add : repr -> repr -> repr
end;;

(* A sample expression *)
(* It is now a functor: an expression is literally parameterized over
   the interpreter.
   The proper expression has the same form as in *)
(* Haskell, see Intro2.hs *)
module TF1(S: ExpSYM) = struct
  open S
  let res = add (lit 8) (neg (add (lit 1) (lit 2)))
end;;

(* Now we define one interpreter *)
(* It is clearly meta-circular *)
module Eval = struct
  type repr = int
  let lit n = n
  let neg e = - e
  let add e1 e2 = e1 + e2
end;;

(* We can evaluate our sample term *)
let 5 = let module M = TF1(Eval) in M.res;;

(* We can define another interpreter *)

module View = struct
  type repr = string
  let lit n = string_of_int n
  let neg e = "(-" ^ e ^ ")"
  let add e1 e2 = "(" ^ e1 ^ " + " ^ e2 ^ ")"
end;;

(* and evaluate the same term using the new interpreter *)
let "(8 + (-(1 + 2)))" = let module M = TF1(View) in M.res;;


(* We can extend our language with a new expression form *)

module type EMSYM = sig
  include ExpSYM			(* Reuse ExpSYM declaration *)
  val mul : repr -> repr -> repr
end;;


(* We extend the interpreters, reusing the old code *)

module EvalM = struct
  include Eval
  let mul e1 e2 = e1 * e2
end;;

module ViewM = struct
  include View
  let mul e1 e2 = "(" ^ e1 ^ " * " ^ e2 ^ ")"
end;;

(* We can evaluate the old expression TF1 using the extended EvalM.
   So, EvalM is fully backwards compatible with Eval 
*)
let 5 = let module M = TF1(EvalM) in M.res;;

(* We can write extended terms *)

(* Again, the code for the expression has literally the same form
  as that in ExtF.hs
*)

module TFM1(S: EMSYM) = struct
  open S
  let res = add (lit 7) (neg (mul (lit 1) (lit 2)))
end;;

module TFM2(S: EMSYM) = struct
  open S
  module Tf1 = TF1(S)
  let res = mul (lit 7) Tf1.res		(* reusing the old expression TF1 *)
end;;

let 5 = let module M = TFM1(EvalM) in M.res;;
let 35 = let module M = TFM2(EvalM) in M.res;;


(* we can't pass Eval to TFM1 by mistake:

let module M = TFM1(Eval) in M.res;;

Error: Signature mismatch:
       Modules do not match:
         sig
           type repr = int
           val lit : 'a -> 'a
           val neg : int -> int
           val add : int -> int -> int
         end
       is not included in
         EMSYM
       The field `mul' is required but not provided

*)

let "(7 + (-(1 * 2)))" = let module M = TFM1(ViewM) in M.res;;
let "(7 * (8 + (-(1 + 2))))" = let module M = TFM2(ViewM) in M.res;;



(* Unlike final_obj.ml, we obtain the extensibility along two axes,
   adding new variants and adding new operations (interpreters)
*)

(* But there is a problem: our sample expressions, TF1, TFM1, TFM2
   are modules -- which are not first-class (although they soon be, in OCaml).
   In any case, first-class modules are rarity, and their theory is complex.
*)

