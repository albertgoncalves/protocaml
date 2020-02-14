module FIFO = struct
    type 'a _node = {
        value : 'a;
        mutable next : 'a _node_t;
    }
    and 'a _node_t = 'a _node ref option
    and 'a t = {
        mutable first : 'a _node_t;
        mutable last : 'a _node_t;
    }

    let construct () : 'a t = {
        first = None;
        last = None;
    }

    let push (l : 'a t) (v : 'a) : unit =
        let next : 'a _node ref option = Some (ref {
            value = v;
            next = None;
        }) in
        match (l.first, l.last) with
            | (Some f, None) ->
                (
                    !f.next <- next;
                    l.last <- next
                )
            | (_, Some l') ->
                (
                    !l'.next <- next;
                    l.last <- next
                )
            | _ -> l.first <- next

    let pop (l : 'a t) : 'a option =
        let f (ptr : 'a _node ref) : 'a =
            let v : 'a = !ptr.value in
            l.first <- !ptr.next;
            if l.first = l.last then
                l.last <- None
            else
                ();
            v in
        Option.map f l.first

    let print (to_string : 'a -> string) (l : 'a t) : unit =
        Printf.fprintf stdout "FIFO.t   : [";
        let rec f (n : 'a _node_t) : unit = Option.iter f' n
        and f' (ptr : 'a _node ref) : unit =
            Printf.fprintf stdout " %s" (to_string !ptr.value);
            f !ptr.next in
        f l.first;
        Printf.fprintf stdout "]\n"
end

let () : unit =
    let l : int FIFO.t = FIFO.construct () in
    let f () : unit =
        match FIFO.pop l with
            | None -> Printf.fprintf stdout "FIFO.pop : None\n"
            | Some v -> Printf.fprintf stdout "FIFO.pop : Some %d\n" v in
    for i = 0 to 10 do
        FIFO.push l i
    done;
    FIFO.print string_of_int l;
    for _ = 0 to 4 do
        f ()
    done;
    FIFO.print string_of_int l;
    for _ = 0 to 6 do
        f ()
    done;
    for i = 0 to 10 do
        FIFO.push l i
    done;
    FIFO.print string_of_int l;
    flush stdout
