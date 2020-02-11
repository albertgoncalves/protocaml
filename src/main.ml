module LIFO = struct
    type 'a t = {value : 'a; mutable pointer : 'a t option ref}

    let push (x : 'a) (xs : 'a t) : unit =
        let ptr : 'a t option = !(xs.pointer) in
        xs.pointer := Some ({
            value = x;
            pointer = ref ptr;
        })

    let pop (xs : 'a t) : ('a * 'a t option) =
        match !(xs.pointer) with
            | None -> (xs.value, None)
            | Some ptr -> (xs.value, Some ptr)

    let print (to_string : 'a -> string) (xs : 'a t) : unit =
        Printf.fprintf stdout "LIFO (";
        let rec f (xs : 'a t) : unit =
            Printf.fprintf stdout "%s" (to_string xs.value);
            match !(xs.pointer) with
                | None -> ()
                | Some ptr -> (
                        Printf.fprintf stdout ", ";
                        f ptr
                    ) in
        f xs;
        Printf.fprintf stdout ")\n"
end

let () : unit =
    let xs : int LIFO.t = {LIFO.value = 0; LIFO.pointer = ref None} in
    LIFO.push 1 xs;
    LIFO.push 2 xs;
    LIFO.push 3 xs;
    let (x, xs) : (int * int LIFO.t option) = LIFO.pop xs in
    (match xs with
        | None -> ()
        | Some xs -> LIFO.print string_of_int xs);
    Printf.fprintf stdout "%d\n%!" x
