exception Yikes
(* env 0, fenv : 'a -> 'b *)

let env0 = fun x -> raise Yikes

(* ext : ('a -> 'b) -> 'a -> 'b -> 'a -> 'b *)

let ext env x v = fun y -> if x = y then v else env y

type exp = Int of int
         | Var of string
         | App of string * exp
         | Add of exp * exp
         | Sub of exp * exp | Mul of exp * exp
         | Div of exp * exp
         | Ifz of exp * exp * exp

type def = Declaration of string * string  * exp
type prog = Program of def list * exp



let prog1 = Program ([Declaration ("fact", "x", Ifz (Var "x",
                Int 1, Mul (Var "x", (App ("fact", Sub(Var "x", Int 1))))))],
                App ("fact", Int 5))
                       
(* eval1 : exp -> (string -> int) -> (string -> int -> int) -> int *)

let rec eval1 e env fenv = 
  match e with
    Int i -> i
  | Var s -> env s
  | App (s, e2) -> (fenv s) (eval1 e2 env fenv)
  | Add (e1, e2) -> (eval1 e1 env fenv) + (eval1 e2 env fenv)
  | Sub (e1, e2) -> (eval1 e1 env fenv) - (eval1 e2 env fenv)
  | Mul (e1, e2) -> (eval1 e1 env fenv) * (eval1 e2 env fenv)
  | Div (e1, e2) -> (eval1 e1 env fenv) / (eval1 e2 env fenv)
  | Ifz (e1,e2,e3) -> if (eval1 e1 env fenv) = 0 
                      then (eval1 e2 env fenv)
                      else (eval1 e3 env fenv)

(* peval1 : prog -> (string -> int) -> (string -> int -> int) -> int *)
let rec peval1 p env fenv = 
    match p with 
      Program([], e) -> eval1 e env fenv
    | Program(Declaration(s1,s2,e1)::tl, e) ->
        let rec f x = eval1 e1 (ext env s2 x) (ext fenv s1 f)
        in peval1 (Program(tl,e)) env (ext fenv s1 f)

let res1 = peval1 prog1 env0 env0

(* eval2 : exp -> (string -> int code) -> (string -> (int -> int) code)  
               -> int code *)

let rec eval2 e env fenv = 
  match e with
    Int i -> .<i>.
  | Var s -> env s
  | App ( s, e2) -> .<.~(fenv s).~(eval2 e2 env fenv)>.
  | Add (e1, e2) -> .<.~(eval2 e1 env fenv) + .~(eval2 e2 env fenv)>.
  | Sub (e1, e2) -> .<.~(eval2 e1 env fenv) - .~(eval2 e2 env fenv)>.
  | Mul (e1, e2) -> .<.~(eval2 e1 env fenv) * .~(eval2 e2 env fenv)>.
  | Div (e1, e2) -> .<.~(eval2 e1 env fenv) / .~(eval2 e2 env fenv)>.
  | Ifz (e1,e2,e3) -> .<if .~(eval2 e1 env fenv) == 0
                        then .~(eval2 e2 env fenv)
                        else .~(eval2 e3 env fenv)>.

(* pval2 : prog -> (string -> int code) -> (string -> (int->int) code)
                -> int code *)

let rec peval2 p env fenv =
  match p with
    Program ([],e) -> eval2 e env fenv
  | Program (Declaration (s1,s2,e1)::tl, e) ->
      .<let rec f x = .~(eval2 e1 (ext  env s2 .<x>.) 
                                  (ext fenv s1 .<f>.))
        in .~(peval2 (Program(tl,e)) env (ext fenv s1 .<f>.))>.

let res2 = peval2 prog1 env0 env0

type 'a option = None | Just of 'a

(* eval 3 : exp -> (string -> int) -> (string -> int -> int option) 
 *              -> int option  *)

let rec eval3 e env fenv = 
  match e with
    Int i -> Some i
  | Var s -> Some (env s)
  | App (s, e2) -> (match (eval3 e2 env fenv ) with
                      Some x -> (fenv s) x
                    | None   -> None)
  | Add (e1,e2) -> (match (eval3 e1 env fenv, eval3 e2 env fenv)
                      with (Some x, Some y) -> Some (x+y)
                       | _ -> None)
  | Sub (e1,e2) -> (match (eval3 e1 env fenv, eval3 e2 env fenv)
                      with (Some x, Some y) -> Some (x-y)
                       | _ -> None)
  | Mul (e1,e2) -> (match (eval3 e1 env fenv, eval3 e2 env fenv)
                      with (Some x, Some y) -> Some (x*y)
                       | _ -> None)
  | Div (e1,e2) -> (match (eval3 e1 env fenv, eval3 e2 env fenv)
                      with (Some x, Some y) -> 
                            if y == 0 then None else Some(x/y)
                       | _ -> None)
  | Ifz (e1,e2,e3) -> (match (eval3 e1 env fenv) with
                         Some x -> if x == 0 then (eval3 e2 env fenv)
                                             else (eval3 e3 env fenv)
                        | None -> None)


let rec peval3 p env fenv = 
    match p with 
      Program([], e) -> eval3 e env fenv
    | Program(Declaration(s1,s2,e1)::tl, e) ->
        let rec f x = eval3 e1 (ext env s2 x) (ext fenv s1 f)
        in peval3 (Program(tl,e)) env (ext fenv s1 f)

let res3 = peval3 prog1 env0 env0

(* eval4 : exp -> (string -> int code) -> (string -> (int -> int option) code)
 * -> (int option) code *)

let rec eval4 e env fenv = 
  match e with 
    Int i -> .<Some i>.
  | Var s -> .<Some .~(env s)>. 
  | App (s,e2) -> .<(match .~(eval4 e2 env fenv)
                       with Some x -> .~(fenv s) x 
                          |   None -> None)>.
  | Add (e1,e2) -> .<(match (.~(eval4 e1 env fenv), .~(eval4 e2 env fenv)) 
                      with  (Some x, Some y) -> Some (x+y) 
                            | _ -> None)>. 
  | Mul (e1,e2) -> .<(match (.~(eval4 e1 env fenv), .~(eval4 e2 env fenv)) 
                      with  (Some x, Some y) -> Some (x*y) 
                            | _ -> None)>. 
  | Sub (e1,e2) -> .<(match (.~(eval4 e1 env fenv), .~(eval4 e2 env fenv)) 
                      with  (Some x, Some y) -> Some (x-y) 
                            | _ -> None)>. 
  | Div (e1,e2) -> .<(match (.~(eval4 e1 env fenv), .~(eval4 e2 env fenv)) 
                      with  (Some x, Some y) -> if y=0 then None else Some (x/y)
                         | _ -> None)>.
  | Ifz (e1,e2,e3) -> .<(match .~(eval4 e1 env fenv) 
                        with Some x -> if x=0 then  .~(eval4 e2 env fenv)
                                              else .~(eval4 e3 env fenv) 
                            | None -> None)>.

let rec peval4 p env fenv =
  match p with
    Program ([],e) -> eval4 e env fenv
  | Program (Declaration (s1,s2,e1)::tl, e) ->
      .<let rec f x = .~(eval4 e1 (ext  env s2 .<x>.) 
                                  (ext fenv s1 .<f>.))
        in .~(peval4 (Program(tl,e)) env (ext fenv s1 .<f>.))>.

let res4 = peval4 prog1 env0 env0

(* eval5 : exp -> (string -> int) -> (string -> int -> int)
 *             -> (int option -> 'a) -> 'a *)

let rec eval5 e env fenv k =
  match e with
    Int i -> k (Some i)
  | Var s -> k (Some (env s))
  | App (s, e2) -> eval5 e2 env fenv (fun r  -> match r with
                                         Some x -> k (Some ((fenv s) x))
                                       | None   -> k None)
  | Add (e1,e2) -> eval5 e1 env fenv 
                    (fun r -> 
                        eval5 e2 env fenv 
                          (fun s -> match (r,s) with
                                (Some x, Some y ) -> k (Some (x+y))
                              | _ -> k None))
  | Sub (e1,e2) -> eval5 e1 env fenv 
                    (fun r -> 
                        eval5 e2 env fenv 
                          (fun s -> match (r,s) with
                                (Some x, Some y ) -> k (Some (x-y))
                              | _ -> k None))
  | Mul (e1,e2) -> eval5 e1 env fenv 
                    (fun r -> 
                        eval5 e2 env fenv 
                          (fun s -> match (r,s) with
                                (Some x, Some y ) -> k (Some (x*y))
                              | _ -> k None))
  | Div (e1,e2) -> eval5 e1 env fenv 
                    (fun r -> 
                        eval5 e2 env fenv 
                          (fun s -> match (r,s) with
                                (Some x, Some y ) -> if y == 0 then k None else k (Some (x/y))
                              | _ -> k None))
  | Ifz (e1,e2,e3) -> eval5 e1 env  fenv
                        (fun r ->  match r with
                               Some x -> if x == 0 then (eval5 e2 env fenv k)
                                                   else (eval5 e3 env fenv k)
                           | None -> k None)


(* pevalK5 : prog -> (string ->int) -> (string -> int -> int) 
  *               -> (int option -> int) -> int *)

(*
let rec pevalK5 p env fenv k = 
  match p with 
    Program  ([],e) -> eval5 e env fenv k
  | Program (Declaration (s1,s2,e1)::tl,e) ->
      let rec f x = eval5 e1 (ext env s2 x) (ext fenv s1 f) k
         in pevalK5 (Program(tl,e)) env (ext fenv s1 f) k
*)

(*let rec  pevalK5 : prog -> (string ->int) -> (string -> int -> int) 
                 -> (int option -> 'a) -> 'a = 
fun p env fenv k ->*)
let rec pevalK5 p env fenv k =  
  match p with 
     Program ([],e) -> eval5 e env fenv k
   | Program (Declaration (s1,s2,e1)::tl,e) -> 
       let rec f x = eval5 e1 (ext env s2 x) (ext fenv s1 f) k 
       in pevalK5 (Program(tl,e)) env (ext fenv s1 f) k

exception Div_by_zero;;

(* peval5 : prog -> (string -> int) -> (string -> int -> int) -> int *)

let peval5 p env fenv = 
  pevalK5 p env fenv (function Some x -> x | None ->raise Div_by_zero)

let res5 = peval5 prog1 env0 env0

(* eval 6 : exp -> (string -> int code) -> (string -> (int -> int) code)
 *              -> (int code option -> 'b code) -> 'b code *)

let rec eval6 e env fenv k = match e with
   Int i -> k (Some .<i>.)
 | Var s -> k (Some (env s))
 | App (s,e2) -> eval6 e2 env fenv
                  (fun r -> match r with
                     Some x -> k (Some .<.~(fenv s) .~x>.)
                   | None -> k None)
 | Add (e1, e2) -> eval6 e1 env fenv (fun r -> 
                     eval6 e2 env fenv (fun s ->
                         match (r,s) with
                           (Some x, Some y) -> k (Some .<.~x + .~y>.)
                        | _ -> k None))
 | Sub (e1, e2) -> eval6 e1 env fenv (fun r -> 
                     eval6 e2 env fenv (fun s ->
                         match (r,s) with
                           (Some x, Some y) -> k (Some .<.~x - .~y>.)
                        | _ -> k None))
 | Mul (e1, e2) -> eval6 e1 env fenv (fun r -> 
                     eval6 e2 env fenv (fun s ->
                         match (r,s) with
                           (Some x, Some y) -> k (Some .<.~x * .~y>.)
                        | _ -> k None))
 | Div (e1, e2) -> eval6 e1 env fenv (fun r -> 
                     eval6 e2 env fenv (fun s ->
                         match (r,s) with
                           (Some x, Some y) -> 
                              .<if .~y == 0 then .~(k None) 
                                            else .~(k (Some .<.~x/ .~y>.))>.
                        | _ -> k None))
 | Ifz (e1,e2,e3) -> eval6 e1 env fenv 
                     (fun r -> match r with
                        Some x -> .<if .~x == 0 then .~(eval6 e2 env fenv k)
                                                else .~(eval6 e3 env fenv k)>.
                     | None -> k None)

(*let rec  pevalK5 : prog -> (string ->int) -> (string -> int -> int) 
                 -> (int option -> 'a) -> 'a = 
fun p env fenv k ->*)
let rec pevalK6 p env fenv k =  
  match p with 
     Program ([],e) -> eval6 e env fenv k
   | Program (Declaration (s1,s2,e1)::tl,e) -> 
       .<let rec f x = .~(eval6 e1 (ext env s2 .<x>.) (ext fenv s1 .<f>.) k)
       in .~(pevalK6 (Program(tl,e)) env (ext fenv s1 .<f>.) k)>.

(* peval6 : prog -> (string -> int code) -> (string -> (int -> int) code)
 *               -> int code *)
let peval6 p env fenv = 
(*    pevalK6 p env fenv (function Some x -> x | None -> .<32>.)*)
    pevalK6 p env fenv (function Some x -> x | None -> .<raise Not_found>.)

let res6 = peval6 prog1 env0 env0

let prog2 = Program ([Declaration ("fact", "x", Ifz (Var "x",
                Int 1, Mul (Var "x", (App ("fact", Sub(Var "x", Int 1))))))],
                App ("fact", Div(Int 20, Int 4 )))

let res6a = peval6 prog2 env0 env0
