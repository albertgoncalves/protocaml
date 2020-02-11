module LinkedList = struct
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
        Printf.fprintf stdout "LinkedList (";
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
    let xs : int LinkedList.t = {
        LinkedList.value = 0;
        LinkedList.pointer = ref None;
    } in
    LinkedList.push 1 xs;
    LinkedList.push 2 xs;
    LinkedList.push 3 xs;
    let (x, xs) : (int * int LinkedList.t option) = LinkedList.pop xs in
    (match xs with
        | None -> ()
        | Some xs -> LinkedList.print string_of_int xs);
    Printf.fprintf stdout "%d\n%!" x
