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
        mutable next : 'a _node option;
    }

    type 'a t = {
        mutable head : 'a _node option;
    }

    let construct () : 'a t = {
        head = None;
    }

    let push (l : 'a t) (v : 'a) : unit =
        let next : 'a _node option = Some {
            value = v;
            next = l.head;
        } in
        l.head <- next

    let pop (l : 'a t) : 'a option =
        let f (node : 'a _node) : 'a =
            let v : 'a = node.value in
            l.head <- node.next;
            v in
        Option.map f l.head

    let pop_at (l : 'a t) (i : int) : 'a option =
        let rec f (prev : 'a _node option) (current : 'a _node option)
            (next : 'a _node option) : int -> 'a option = function
            | 0 ->
                (match (prev, current, next) with
                    | (None, Some c, (Some _ as n)) ->
                        (
                            l.head <- n;
                            Some c.value
                        )
                    | (Some p, Some c, (Some _ as n)) ->
                        (
                            p.next <- n;
                            Some c.value
                        )
                    | _ -> None)
            | i ->
                (match (current, next) with
                    | ((Some _ as c), (Some n as n')) ->
                        f c n' n.next (i - 1)
                    | _ -> None) in
        let f' (node : 'a _node) : 'a option =
            f None (Some node) node.next i in
        Option.bind l.head f'

    let print (to_string : 'a -> string) (l : 'a t) : unit =
        let rec f : 'a _node option -> unit = function
            | None -> ()
            | Some node ->
                (
                    Printf.fprintf stdout " %s" (to_string node.value);
                    f node.next
                ) in
        Printf.fprintf stdout "LinkedList.t        : [";
        f l.head;
        Printf.fprintf stdout "]\n"
end

let () : unit =
    let l : int LinkedList.t = LinkedList.construct () in
    for i = 0 to 10 do
        LinkedList.push l i
    done;
    LinkedList.print string_of_int l;
    let f (i : int) : unit =
        match LinkedList.pop_at l i with
            | None -> Printf.fprintf stdout "LinkedList.pop_at %d : None\n" i
            | Some v ->
                Printf.fprintf stdout "LinkedList.pop_at %d : Some %d\n" i v in
    f 4;
    f 2;
    f 7;
    f 7;
    f 1;
    f 0;
    LinkedList.print string_of_int l;
    let v : int option ref = ref (LinkedList.pop l) in
    while !v <> None do
        let v' : int = Option.get !v in
        Printf.fprintf stdout "LinkedList.pop      : Some %d\n" v';
        v := LinkedList.pop l
    done;
    LinkedList.print string_of_int l;
    LinkedList.push l 11;
    LinkedList.print string_of_int l;
    flush stdout
