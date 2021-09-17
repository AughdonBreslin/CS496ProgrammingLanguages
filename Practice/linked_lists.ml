type 'a option = None | Some of 'a

type 'a node = { mutable data: 'a;
                 mutable next: ('a node) option}

type 'a ll = { mutable head: ('a node) option;
               mutable size: int }

let l : 'a ll = {
    head = None; (* What stands in for a null value? Something like nullptr *)
    size = 0
}

(* 1 -> 2 -> 3 *)
let l2 : int ll = {
    head = Some({data=1;
                 next = Some ({data=2;
                               next = Some({data=3;
                                            next=None})})});
    size = 3
}

let string_of_ll : ('a -> string) -> 'a ll -> string =
    fun f l ->
    let rec helper node =
        match node with
        | None -> ""
        | Some n ->
            f n.data ^ "," ^ helper n.next
    in 
    "["^helper l.head^"]"

(*
    EQUIVALENT JAVA CODE:

    private static int sum(Node<Integer> n) {
        if (n == null) {
            return 0;
        }
        return n.data + sum(n.next);
    }

    public static int sum(LL<Integer> l) {
        return sum(l.head);
    }
*)
let sum : int ll -> int =
    fun l ->
    let rec helper node =
        match node with
        | None -> 0
        | Some n -> n.data + helper n.next
    in
    helper l.head

(* 
    private static void bump(Node<Integer> n) {
        if (n == null) {
            return 0;
        }
        n.data++;
    }

    public static void bump(LL<Integer> l) {
        return bump(l.head);
    }
*)
let bump : int ll -> unit =
    fun l ->
    let rec helper node =
        (* this is of type unit *)
        match node with
        | None -> ()
        | Some n ->
            n.data <- n.data+1;
            helper n.next
    in
    helper l.head

(* 
    class LL<A> {
        static class Node<B> {
            B data;
            Node<B> next;
            ...
        }
        Node<A> head;
        int size;

        LL() {
            this.head = null;
            this.size = 0;
        }
    }

    utop # List.assoc_opt;;
    - : 'a -> ('a * 'b) list -> 'b option = <fun>
*)

(* Exercises *)
let ll_to_list : 'a ll -> 'a list =
    fun l ->
    let rec helper node =
        match node with
        | None -> []
        | Some n -> n.data :: helper n.next
    in 
    helper l.head

(* the assignment operator <- cannot change types of what is being changed. *)
let map : ('a -> 'a) -> 'a ll -> unit =
    fun f l ->
    let rec helper node =
        match node with
        | None -> ()
        | Some n ->
            n.data <- f (n.data);
            helper n.next
    in
    helper l.head
    
(* STUFF FOR QUIZ 5 *)
(* Matthew Oyales & Aughdon Breslin *)

let new_node : 'a -> 'a node  =
    fun i ->
    {data=i; next=None}

(* Get the n-th element of a list *)
let get : int -> 'a ll -> 'a option =
    fun i l ->
    let rec helper i node =
        match node with
        | None -> None
        | Some n ->
            if i=0 then Some n.data
            else helper (i-1) n.next
    in
    helper i l.head

(* Adds to beginning of list. *)
let add : 'a -> 'a ll -> unit =
    fun e l ->
    let new_n = (new_node e)
    in
    let helper node =
        match node with
        | None ->
            l.head <- Some(new_n);
            l.size <- (l.size + 1)
        | Some n ->
            new_n.next <- l.head;
            l.head <- Some(new_n);
            l.size <- (l.size + 1)
    in 
    helper l.head

let add_at : int -> 'a -> 'a ll -> unit =
    fun i e l ->
    if i = 0 || l.size = 0
        then add e l
    else
        let new_n = (new_node e)
        in
            let rec helper i no =
                match no.next with
                | None ->
                    no.next <- Some(new_n);
                    l.size <- (l.size + 1)
                | Some n ->
                    if i = 1 then
                        (no.next <- Some({data=e;next=Some(n)});
                         l.size <- (l.size + 1))
                    else helper (i-1) n
        in 
        helper i l.head

(* Removes first*)
let remove : 'a ll -> unit =
    fun l ->
    let helper node =
        match node with
        | None -> ()
        | Some n ->
            l.head <- n.next;
            l.size <- (l.size - 1)
    in
    helper l.head

let remove_at : 'a -> 'a ll -> unit =
    fun i l ->
    (*if i = 0 || l.size = 0
        then remove l
    else
        let curr = l.head
        in 
        let temp = l.head
        let rec helper i no =
            match no.next with
            | None -> ()
            | Some n -> 
                if i = 0
                then
                (curr <- )
                else helper (i-1) n.next
        in
        helper i l.head*)
    failwith "implement"
