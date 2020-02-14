module type LinkedList_t = sig
    type _ t
    val construct : unit -> 'a t
    val push : 'a t -> 'a -> unit
    val pop : 'a t -> 'a option
    val pop_at : 'a t -> int -> 'a option
    val print : ('a -> string) -> 'a t -> unit
end

module LinkedList : LinkedList_t = struct
    type 'a _node = {
        value : 'a;
        mutable next : 'a _node_t;
    }
    and 'a _node_t = 'a _node ref option
    and 'a t = {
        mutable nodes : 'a _node_t;
    }

    let construct () : 'a t = {
        nodes = None;
    }

    let push (l : 'a t) (v : 'a) : unit =
        let next : 'a _node ref option = Some (ref {
            value = v;
            next = l.nodes;
        }) in
        l.nodes <- next

    let pop (l : 'a t) : 'a option =
        let f (ptr : 'a _node ref) : 'a =
            let v : 'a = !ptr.value in
            l.nodes <- !ptr.next;
            v in
        Option.map f l.nodes

    let pop_at (l : 'a t) (i : int) : 'a option =
        let rec f (prev : 'a _node_t) (current : 'a _node_t)
            (next : 'a _node_t) : int -> 'a option = function
            | 0 ->
                (match (prev, current, next) with
                    | (None, Some c, (Some _ as n)) ->
                        (
                            l.nodes <- n;
                            Some !c.value
                        )
                    | (Some p, Some c, (Some _ as n)) ->
                        (
                            !p.next <- n;
                            Some !c.value
                        )
                    | _ -> None)
            | i ->
                (match (current, next) with
                    | ((Some _ as c), (Some n as n')) ->
                        f c n' !n.next (i - 1)
                    | _ -> None) in
        let f' (ptr : 'a _node ref) : 'a option =
            f None (Some ptr) !ptr.next i in
        Option.bind l.nodes f'

    let print (to_string : 'a -> string) (l : 'a t) : unit =
        Printf.fprintf stdout "LinkedList.t        : [";
        let rec f (n : 'a _node_t) : unit = Option.iter f' n
        and f' (ptr : 'a _node ref) : unit =
            Printf.fprintf stdout " %s" (to_string !ptr.value);
            f !ptr.next in
        f l.nodes;
        Printf.fprintf stdout "]\n"
end

let () : unit =
    let l : int LinkedList.t = LinkedList.construct () in
    let f (i : int) : unit =
        match LinkedList.pop_at l i with
            | None -> Printf.fprintf stdout "LinkedList.pop_at %d : None\n" i
            | Some v ->
                Printf.fprintf stdout "LinkedList.pop_at %d : Some %d\n" i v in
    for i = 0 to 10 do
        LinkedList.push l i
    done;
    LinkedList.print string_of_int l;
    f 4;
    f 2;
    f 9;
    LinkedList.print string_of_int l;
    for _ = 0 to 9 do
        match LinkedList.pop l with
            | None -> Printf.fprintf stdout "LinkedList.pop      : None\n"
            | Some v ->
                Printf.fprintf stdout "LinkedList.pop      : Some %d\n" v;
    done;
    LinkedList.print string_of_int l;
    LinkedList.push l 11;
    LinkedList.print string_of_int l;
    flush stdout
