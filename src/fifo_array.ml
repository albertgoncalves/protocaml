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
        memory : 'a option array;
        mutable index : int;
        mutable remaining_cap : int
    }

    let construct (n : int) : 'a t = {
        capacity = n;
        memory = Array.make n None;
        index = 0;
        remaining_cap = n;
    }

    let push (queue : 'a t) (v : 'a) : bool =
        if queue.remaining_cap = 0 then
            false
        else (
            queue.memory.(queue.index) <- Some v;
            queue.index <- (queue.index + 1) mod queue.capacity;
            queue.remaining_cap <- queue.remaining_cap - 1;
            true
        )

    let pop (queue : 'a t) : 'a option =
        if queue.capacity <= queue.remaining_cap then
            None
        else (
            let i : int =
                (queue.index + queue.remaining_cap) mod queue.capacity in
            let v : 'a option = queue.memory.(i) in
            queue.memory.(i) <- None;
            queue.remaining_cap <- queue.remaining_cap + 1;
            v
        )

    let print (to_string : 'a -> string) (queue : 'a t) : unit =
        let f : 'a option -> unit = function
            | None -> Printf.fprintf stdout " None"
            | Some v -> Printf.fprintf stdout " (Some %s)" (to_string v) in
        Printf.fprintf stdout "[";
        Array.iter f queue.memory;
        Printf.fprintf stdout "]\n"
end

let () : unit =
    let queue : int FIFO.t = FIFO.construct 8 in
    let f () : unit =
        match FIFO.pop queue with
            | None -> Printf.fprintf stdout "FIFO.pop : None\n"
            | Some v -> Printf.fprintf stdout "FIFO.pop : Some %d\n" v in
    let i : int ref = ref 0 in
    while FIFO.push queue !i do
        incr i
    done;
    FIFO.print string_of_int queue;
    for _ = 4 downto 0 do
        f ()
    done;
    FIFO.print string_of_int queue;
    while FIFO.push queue !i do
        incr i
    done;
    FIFO.print string_of_int queue;
    for _ = 4 downto 0 do
        f ()
    done;
    FIFO.print string_of_int queue;
    while FIFO.push queue !i do
        incr i
    done;
    FIFO.print string_of_int queue;
    while FIFO.pop queue <> None; do
        ()
    done;
    FIFO.print string_of_int queue;
    flush stdout
