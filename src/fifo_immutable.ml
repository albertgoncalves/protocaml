module Fifo = struct
    type 'a t = {
        input : 'a list;
        output : 'a list;
    }

    let push (xs : 'a t) (x : 'a) : 'a t = {xs with input = x :: xs.input}

    let rec pop (xs : 'a t) : ('a * 'a t) option =
        match (xs.input, xs.output) with
            | ([], []) -> None
            | (_, x :: o) -> Some (x, {xs with output = o})
            | (i, []) -> pop {input = []; output = List.rev i}

    let peek (xs : 'a t) : 'a option =
        match (xs.input, xs.output) with
            | ([], []) -> None
            | (_, x :: _) -> Some x
            | (i, []) -> match List.rev i with
                | (x :: _) -> Some x
                | [] -> None
    let flush (xs : 'a t) : 'a t =
        {input = []; output = List.rev xs.input |> List.append xs.output}
end

let rec unfold_right (f : ('b -> ('a * 'b) option)) (b : 'b) : 'a list =
    match f b with
        | None -> []
        | Some (a, b) -> a :: unfold_right f b

let () : unit =
    let xs : char Fifo.t =
        ['a'; 'b'; 'c'; 'd'; 'e'; 'f']
        |> List.fold_left Fifo.push {Fifo.input = []; Fifo.output = []} in
    Fifo.flush xs |> Fifo.peek |> Option.iter (Printf.printf "Fifo.peek %C\n");
    Fifo.flush xs
    |> unfold_right Fifo.pop
    |> List.iter (Printf.printf "Fifo.pop %C\n")
