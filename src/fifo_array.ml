module type FIFO_t = sig
    type _ t
    val construct : int -> 'a t
    val push : 'a t -> 'a -> bool
    val pop : 'a t -> 'a option
    val print : ('a -> string) -> 'a t -> unit
end

module FIFO : FIFO_t = struct
    type 'a t = {
        capacity : int;
        mutable memory : 'a option array;
        mutable index : int;
        mutable remaining_cap : int
    }

    let construct (n : int) : 'a t = {
        capacity = n;
        memory = Array.make n None;
        index = 0;
        remaining_cap = n;
    }

    let push (l : 'a t) (v : 'a) : bool =
        if l.remaining_cap = 0 then
            false
        else
            (
                l.memory.(l.index) <- Some v;
                l.index <- (l.index + 1) mod l.capacity;
                l.remaining_cap <- l.remaining_cap - 1;
                true
            )

    let pop (l : 'a t) : 'a option =
        if l.capacity <= l.remaining_cap then
            None
        else
            (
                let i : int = (l.index + l.remaining_cap) mod l.capacity in
                let v : 'a option = l.memory.(i) in
                l.memory.(i) <- None;
                l.remaining_cap <- l.remaining_cap + 1;
                v
            )

    let print (to_string : 'a -> string) (l : 'a t) : unit =
        Printf.fprintf stdout "[";
        let f : 'a option -> unit = function
            | None -> Printf.fprintf stdout " None"
            | Some v -> Printf.fprintf stdout " (Some %s)" (to_string v) in
        Array.iter f l.memory;
        Printf.fprintf stdout "]\n"

end

let () : unit =
    let l : int FIFO.t = FIFO.construct 8 in
    let f () : unit =
        match FIFO.pop l with
            | None -> Printf.fprintf stdout "FIFO.pop : None\n"
            | Some v -> Printf.fprintf stdout "FIFO.pop : Some %d\n" v in
    let i : int ref = ref 0 in
    while FIFO.push l !i do
        incr i
    done;
    FIFO.print string_of_int l;
    for _ = 3 downto 0 do
        f ()
    done;
    FIFO.print string_of_int l;
    for _ = 4 downto 0 do
        f ()
    done;
    FIFO.print string_of_int l;
    while FIFO.push l !i do
        incr i
    done;
    FIFO.print string_of_int l;
    while FIFO.pop l <> None; do
        ()
    done;
    FIFO.print string_of_int l;
    flush stdout
