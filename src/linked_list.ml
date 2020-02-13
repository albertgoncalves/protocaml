module LinkedList = struct
    type 'a _node = {
        value : 'a;
        mutable next : 'a _node ref option;
    }

    and 'a _node_t = 'a _node ref option

    type 'a t = {
        mutable nodes : 'a _node_t;
    }

    let construct (() : unit) : 'a t = {
        nodes = None;
    }

    let push (l : 'a t) (v : 'a) : unit =
        let next : 'a _node ref = ref {
            value = v;
            next = l.nodes;
        } in
        l.nodes <- Some next

    let pop (l : 'a t) : 'a option =
        match l.nodes with
            | None -> None
            | Some ptr -> (
                    let v : 'a = !ptr.value in
                    l.nodes <- !ptr.next;
                    Some v
                )

    let pop_at (l : 'a t) (i : int) : 'a option =
        let rec f (prev : 'a _node_t) (current : 'a _node_t)
            (next : 'a _node_t) : int -> 'a option = function
            | 0 -> (
                    match (prev, current, next) with
                        | (None, Some c, (Some _ as n)) -> (
                                l.nodes <- n;
                                Some !c.value
                            )
                        | (Some p, Some c, (Some _ as n)) -> (
                                !p.next <- n;
                                Some !c.value
                            )
                        | _ -> None
                )
            | i -> (
                    match (current, next) with
                        | (Some _ as c, (Some n as n')) -> (
                                f c n' !n.next (i - 1)
                            )
                        | _ -> None
                ) in
        match l.nodes with
            | None -> None
            | Some ptr -> f None (Some ptr) !ptr.next i

    let print (to_string : 'a -> string) (l : 'a t) : unit =
        Printf.fprintf stdout "LinkedList.t        : [";
        let rec f (n : 'a _node_t) : unit =
            match n with
                | None -> ()
                | Some ptr -> (
                        Printf.fprintf stdout " %s" (to_string !ptr.value);
                        f !ptr.next
                    ) in
        f l.nodes;
        Printf.fprintf stdout "]\n"
end

let () : unit =
    let l : int LinkedList.t = LinkedList.construct () in
    for i = 0 to 10 do
        LinkedList.push l i
    done;
    (match LinkedList.pop l with
        | None -> Printf.fprintf stdout "LinkedList.pop      : None\n"
        | Some v -> Printf.fprintf stdout "LinkedList.pop      : Some %d\n" v);
    (match LinkedList.pop_at l 4 with
        | None -> Printf.fprintf stdout "LinkedList.pop_at 4 : None\n"
        | Some v -> Printf.fprintf stdout "LinkedList.pop_at 4 : Some %d\n" v);
    (match LinkedList.pop_at l 9 with
        | None -> Printf.fprintf stdout "LinkedList.pop_at 9 : None\n"
        | Some v -> Printf.fprintf stdout "LinkedList.pop_at 9 : Some %d\n" v);
    LinkedList.print string_of_int l;
    flush stdout
