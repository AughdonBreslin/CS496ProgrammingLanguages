type fruit = A | O | K (* Apple, Orange, Kiwi *)

type 'a result = Error of string | Ok of 'a

let b1 : fruit list = [O;A;A;A;O;K;K;K;K;A;O;O]

(*
    A fruit basket is an expression of type:
        fruit list

    A fruit basket inspector (FBI) is an expression of type:
        fruit list -> int result
*)

(* 3 basic FBI examples *)
let apples : fruit list -> int result = 
    fun b ->
    Ok (List.length (List.filter (fun f -> f=A) b))

let oranges : fruit list -> int result = 
    fun b ->
    Ok (List.length (List.filter (fun f -> f=O) b))

let kiwis : fruit list -> int result = 
    fun b ->
    Ok (List.length (List.filter (fun f -> f=K) b))

(*
    A fruit basket processor (FBP) is an expression of type:
        fruit list -> (fruit list) result
        (You get a new fruit basket)

    Summary:
        fruit list -> int result          <-- FBI
        fruit list -> (fruit list) result <-- FBP
*)
let remove_apples : fruit list -> (fruit list) result =
    fun b ->
    Ok (List.filter (fun f -> f!=A) b)

let remove_oranges : fruit list -> (fruit list) result =
    fun b ->
    Ok (List.filter (fun f -> f!=O) b)

let drop_first_fruit : fruit list -> (fruit list) result =
    fun b ->
    if b=[]
        then Error "Basket is empty" (* Not a runtime error, it's a valid result! *)
    else 
        Ok (List.tl b)

(*
    Combining FBIs.
*)

let add_fbi : (fruit list -> int result) -> (fruit list -> int result) 
              -> (fruit list -> int result) = 
    fun fbi1 fbi2 ->
    fun b ->
    match fbi1 b with 
    | Error s -> Error s
    | Ok i -> 
        (match fbi2 b with
        | Error s -> Error s
        | Ok j -> Ok(i+j))

let mul_fbi : (fruit list -> int result) -> (fruit list -> int result) 
              -> (fruit list -> int result) = 
    fun fbi1 fbi2 ->
    fun b ->
    match fbi1 b with 
    | Error s -> Error s
    | Ok i -> 
        (match fbi2 b with
        | Error s -> Error s
        | Ok j -> Ok(i*j)) (* <--- THE ONLY DIFFERENCE *)

(* This is a generic FBI combinator *)
(* Handles error propagation and fruit baskets too *)
let (>>=) : (fruit list -> int result) -> (int -> (fruit list -> int result)) 
            -> fruit list -> int result =
    fun fbi f ->
    fun b ->
    match fbi b with
    | Error s -> Error s
    | Ok i -> f i b

let apples_and_oranges = add_fbi apples oranges

let apples_and_oranges' =
    apples >>= fun i ->
    oranges >>= fun j ->
    (fun _ -> Ok(i+j))

let apples_times_oranges =
    apples >>= fun i ->
    oranges >>= fun j ->
    (fun _ -> Ok(i*j))