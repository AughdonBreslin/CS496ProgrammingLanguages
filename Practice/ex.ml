
(* 
   Examples of recursion 
   Lecture - 25 May 2021
*)

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
  
(* ******************** *)
(* Some useful function schemes *)
(* ******************** *)


(* Map *)
    
let inc i = i+1
let upper c = Char.uppercase_ascii c
let is_zero i = i=0
                
let rec succl : int list -> int list =
  fun l ->
  match l with
  | [] -> []
  | h::t -> inc h :: succl t

let rec upperl : char list -> char list =
  fun l ->
  match l with
  | [] -> []
  | h::t -> upper h :: upperl t
              
let rec is_zerol : int list -> bool list =
  fun l ->
  match l with
  | [] -> []
  | h::t -> is_zero h :: is_zerol t

let rec map : ('a -> 'b) -> 'a list -> 'b list =
  fun f l ->
  match l with
  | [] -> []
  | h::t -> f h :: map f t

let succl' = map inc
let upperl' = map upper              
let is_zerol'  = map is_zero

let is_positive i = i>0
let is_upper c = c=Char.uppercase_ascii c
let is_nonempty l = l!=[]

(* Filter *)
                       
let rec gtz : int list -> int list =
  fun l ->
  match l with
  | [] -> []
  | h::t ->
    if is_positive h
    then h::gtz t
    else gtz t

let rec uppercase : char list -> char list =
  fun l ->
  match l with
  | [] -> []
  | h::t ->
    if is_upper h
    then h::uppercase t
    else uppercase t

let rec non_empty : 'a list list -> 'a list list =
  fun l ->
  match l with
  | [] -> []
  | h::t ->
    if is_nonempty h
    then h::non_empty t
    else non_empty t

let rec filter : ('a -> bool) -> 'a list -> 'a list =
  fun p l ->
  match l with
  | [] -> []
  | h::t ->
    if p h
    then h::filter p  t
    else filter p  t

let gtz' = filter is_positive
let uppercase' = filter is_upper
let non_empty' l = filter is_nonempty l 

(* Fold *)
    
let rec suml : int list -> int =
  fun l ->
  match l with
  | [] -> 0
  | h::t -> h + suml t

let rec andl : bool list -> bool =
  fun l ->
  match l with
  | [] -> true
  | h::t -> h && andl t
    
let rec concat : 'a list list -> 'a list =
  fun l ->
  match l with
  | [] -> []
  | h::t -> h @ concat t


let rec foldr : ('a -> 'b -> 'b) -> 'b -> 'a list -> 'b  =
  fun f a l ->
  match l with
  | [] -> a
  | h::t -> f h (foldr f a t)

let suml' = foldr (fun i r -> i+r) 0
let andl' = foldr (fun i r -> i && r) true   
let concat' = foldr (fun i r -> i @ r) []
    
let rec foldl : ('a -> 'b -> 'b) -> 'b -> 'a list -> 'b =
  fun f a l ->
  match l with
  | [] -> a
  | h::t -> foldl f (f a h) t

(* Algebraic Data Types *)

type color = Red | Green | Blue

let next : color -> color =
  fun c ->
  match c with
  | Red -> Green
  | Green -> Blue
  | Blue -> Red

let rec lookup : 'a -> ('a*'b) list -> 'b  =
  fun key dict ->
  match dict with
  | [] -> raise Not_found
  | (k,v)::t ->
    if k=key
    then v
    else lookup key t

type 'a option = None | Some of 'a
               
let rec lookup' : 'a -> ('a*'b) list -> 'b option  =
  fun key dict ->
  match dict with
  | [] -> None
  | (k,v)::t ->
    if k=key
    then Some v
    else lookup' key t

type ('a,'b) either = Left of 'a | Right of 'b

type 'a btree = Empty |  Node of 'a * 'a btree * 'a btree

let t1 : int btree =
  Node(12,
       Node(7,Empty,Empty),
       Node(33,
            Node(24,Empty,Empty),
            Empty))

let t2 : string btree =
  Node("hello",Empty,Empty)

let rec sizet : 'a btree -> int =
  fun t ->
  match t with
  | Empty -> 0
  | Node(_,lt,rt) -> 1 + sizet lt + sizet rt

let is_leaf : 'a btree -> bool =
  fun t -> 
  match t with
  | Node(_,Empty,Empty) -> true
  | _ -> false

let rec mirror : 'a btree -> 'a btree =
  fun t ->
  match t with
  | Empty -> Empty
  | Node(d,lt,rt) -> Node(d,mirror rt,mirror lt)
