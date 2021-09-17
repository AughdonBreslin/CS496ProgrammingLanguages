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

(**Preorder traversal *)
let rec pre t =
  match t with
  | Empty -> []
  | Node(d,lt,rt) -> [d] @ pre lt @ pre rt

(**Inorder traversal *)
let rec io t =
  match t with
  | Empty -> []
  | Node(d,lt,rt) -> io lt @ [d] @ io rt

(**Postorder traversal *)
let rec post t =
  match t with
  | Empty -> []
  | Node(d,lt,rt) -> post lt @ post rt @ [d]

let rec mapt : ('a -> 'b) -> 'a btree -> 'b btree =
  fun f t ->
  match t with
  | Empty -> Empty
  | Node(d,lt,rt) -> Node(f d, mapt f lt, mapt f rt)

let rec foldt : 'b -> ('a -> 'b -> 'b -> 'b) -> 'a btree -> 'b =
  fun a f t ->
  match t with
  | Empty -> a 
  | Node(d,lt,rt) -> f d (foldt a f lt) (foldt a f rt)