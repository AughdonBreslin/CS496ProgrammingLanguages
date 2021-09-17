(* ******************** *)
(* Recursion on numbers *)
(* ******************** *)

(** [fact n] returns the factorial of [n].
    Precond: The parameter [n] must be positive *)
let rec fact n =
  match n with
  | 0 -> 1
  | m -> m*fact (m-1)

let rec summation n =
  match n with
  | 0 -> 0
  | m -> m + summation (m-1)

(* ******************** *)
(* Recursion on lists *)
(* ******************** *)

let rec length (l:'a list) : int =
  match l with
  | [] -> 0
  | h::t -> 1 + length t

let rec sum (l:int list) : int =
  match l with
  | [] -> 0 
  | h::t -> h + sum t

let rec mem (e:'a) (l:'a list) : bool = 
  match l with
  | [] -> false
  | h::t -> e=h || mem e t

let rec mem' : 'a -> 'a list -> bool =
  fun e l -> 
  match l with
  | [] -> false
  | h::t -> e=h || mem' e t

let rec stutter l =
  match l with
  | [] -> []
  | h::t -> h::h::stutter t

(** [repeat e n] produces a list with [n] copies of the element
 ** [e]. If [n] is negative, then it produces the empty list.
 Eg. repeat "hello" 3 => ["hello";"hello";"hello"] *) 
let rec repeat : 'a -> int -> 'a list =
  fun e n ->
  match n with
  | 0 -> []
  | m when m>0 -> e::repeat e (m-1)
  | _ -> [] (* n is negative *)

(** Like [stutter] except it makes [n] copies of each element *)
    
let rec stuttern : int -> 'a list -> 'a list =
  fun n l ->
  match l with
  | [] -> []
  | h::t -> repeat h n @ stuttern n t

(* Other examples of pattern matching against lists *)

let hd l =
  match l with
  | h::t -> h
  | _ -> failwith "hd: empty list"

(* remove adjacent duplicates

rad [1;2;2;2;3;3;1;1;4;4;4] 
=>
[1;2;3;1;4]

*)

let rec rad l =
  match l with
  | [] -> []      (* l is empty *)
  | [x] -> [x]    (* l has one element *)
  | h1::h2::t ->  (* l has two or more elements *)
    if h1=h2
    then rad (h2::t)
    else h1 :: rad (h2::t)

let rec rev l =
  match l with
  | [] -> []
  | h::t -> rev t @ [h]

let fast_rev l =
  let rec helper l a =
    match l with
    | [] -> a
    | h::t -> helper t (h::a)
  in helper l []
   

let rec has_duplicates l =
  match l with
  | [] -> false
  | h::t -> List.mem h t || has_duplicates t

    
let rec sublists (l:'a list) : 'a list list =
  failwith "implement me"