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