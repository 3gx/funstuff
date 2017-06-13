(* When things go wrong *)

let rec take n l = 
  match l with 
     [] -> 
        if n == 0
          then []
          else raise (Invalid_argument "take")
  | h::t ->
       if n < 0 then raise (Invalid_argument "take") else
          if n == 0 then [] else h :: take (n-1) t


let safe_divide x y = 
  try x / y with
    Division_by_zero -> 0

let rec last l = 
  match l with 
    [] -> raise Not_found
  | [x] -> x
  | _::t -> last t

let p = (1,4)
let q = (1,'1')

let fst p = match p with (x,_) -> x
let snd p = match p with (_,y) -> y

let census = [(1,4); (2,2); (3,2); (4,3); (5,1); (6,2);]

let rec lookup x l = 
  match l with
   [] -> raise Not_found
| (k,v) :: t -> 
      if k == x then v else lookup x t

let rec add k v d = 
  match d with
   [] -> [(k,v)]
  | (k',v')::t ->
       if k == k' 
       then (k,v)::t
       else (k',v')::add k v t


let rec remove k d = 
  match d with
    [] -> []
  | (k',v')::t ->
      if k == k'
      then t
      else (k',v')::remove k t

let key_exists k d = 
  try 
    let _ = lookup k d in true
  with
    Not_found -> false
