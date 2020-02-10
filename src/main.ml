module LIFO = struct
    type 'a t =
        | Nil
        | Pair of 'a * 'a t

    let push (x : 'a) : ('a t -> 'a t) = function
        | Nil -> Pair (x, Nil)
        | Pair (_, _) as ys -> Pair (x, ys)

    let pop : ('a t -> ('a option * 'a t)) = function
        | Nil -> (None, Nil)
        | Pair (x, xs) -> (Some x, xs)

    let len : ('a t -> int) =
        let rec f (n : int) : ('a t -> int) = function
            | Nil -> n
            | Pair (_, xs) -> f (n + 1) xs in
        f 0

    let rec concat (xs : 'a t) : ('a t -> 'a t) = function
        | Nil -> xs
        | Pair (y, ys) -> concat (Pair (y, xs)) ys

    let remove (n : int) : ('a t -> 'a option * 'a t) =
        let rec f (n' : int) (xs : 'a t) (ys : 'a t) : ('a option * 'a t) =
            match (n', ys) with
                | (_, Nil) -> (None, xs)
                | (0, Pair (y, ys)) -> (Some y, concat ys xs)
                | (n', Pair (y, ys)) -> f (n' - 1) (Pair (y, xs)) ys in
        f n Nil

    let print (to_string : 'a -> string) (xs : 'a t) : unit =
        Printf.fprintf stdout "LIFO (";
        let rec f : ('a t -> unit) = function
            | Nil -> ()
            | Pair (x, Nil) -> Printf.fprintf stdout "%s" (to_string x)
            | Pair (x, xs) -> (
                    Printf.fprintf stdout "%s->" (to_string x);
                    f xs
                ) in
        f xs;
        Printf.fprintf stdout ")\n"
end

module Option = struct
    include Option
    include struct
        let print (to_string : 'a -> string) : ('a option -> unit) = function
            | None -> Printf.fprintf stdout "None"
            | Some x -> Printf.fprintf stdout "Some (%s)\n" (to_string x)
    end
end

let () : unit =
    let debrief_int_lifo (xs : int LIFO.t) (label : string) : unit =
        Printf.fprintf stdout "%s       : " label;
        LIFO.print string_of_int xs;
        Printf.fprintf stdout "%s.len() : %d\n" label (LIFO.len xs) in
    let xs : int LIFO.t =
        LIFO.Nil
        |> LIFO.push 1
        |> LIFO.push 2
        |> LIFO.push 3
        |> LIFO.push 4
        |> LIFO.push 5 in
    let (z, zs) : (int option * int LIFO.t) = LIFO.remove 2 xs in
    let (y, ys) : (int option * int LIFO.t) = LIFO.pop xs in
    debrief_int_lifo xs "xs";
    debrief_int_lifo ys "ys";
    debrief_int_lifo zs "zs";
    Printf.fprintf stdout "y        : ";
    Option.print string_of_int y;
    Printf.fprintf stdout "z        : ";
    Option.print string_of_int z
