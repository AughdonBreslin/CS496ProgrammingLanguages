(** Quiz 1: Matthew Oyales & Audie Breslin *)


(* Sample Directed Graph *)

let ex = [(1, 2); (2, 3); (3, 1); (3, 4)]


(*
  1 <---- 3
  |     /\ |
  |    /   | 
 \/  /    \/
  2       4
*)
         
(* 
Eg. outgoing ex 3 => [1,4] 
*)
         
let rec outgoing_nodes g n =
  match g with
  | [] -> []
  | h::t -> 
    if (fst h = n) then snd h :: outgoing_nodes t n
    else outgoing_nodes t n

(* 
   The list of nodes of the graph without duplicates. The order of the
   nodes in the list is irrelevant.
   eg. nodes ex => [1,2,3,4] 
*)
let rec mem (e:'a) (l:'a list) : bool = 
  match l with
  | [] -> false
  | h::t -> e=h || mem e t

let rec nodes g =
  match g with
  | [] -> []
  | h::t ->
    if ((mem (fst h) (nodes t)) = false) then
       if ((mem (snd h) (nodes t)) = false) then 
          fst h :: snd h :: nodes t
       else fst h :: nodes t
    else nodes t
    
(* 
   Remove a node from the graph
   Eg. remove ex 2 =>  [(3, 1); (3, 4)] 
*)
    
let rec remove g n =
  match g with
  | [] -> []
  | h::t ->
    if ((fst h = n) || (snd h = n)) then 
      remove t n
    else h :: remove t n
    
(* Reachable nodes from a source node. (Extra-credit)
   Eg. reachale ex 3 => [1,4,2,3] 
   *)

let diff k l =
  match k with
  | [] -> []
  | h::t when List.mem h l -> diff t l
  | h::t -> h::diff t l

(* Standard BFS traversal of a graph *)
let rec reachable g n =
  let rec reach_help visited cur =
    match cur with
    | [] -> visited
    | h::t ->
      let outgoing = outgoing_nodes g h
        in reach_help (h::visited) (diff outgoing visited @ t)
    in reach_help [] [n]   

