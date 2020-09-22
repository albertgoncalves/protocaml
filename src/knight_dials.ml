type t = {
    key : int;
}

let move : t -> t list = function
    | {key = 0} -> [{key = 4}; {key = 6}]
    | {key = 1} -> [{key = 6}; {key = 8}]
    | {key = 2} -> [{key = 7}; {key = 9}]
    | {key = 3} -> [{key = 4}; {key = 8}]
    | {key = 4} -> [{key = 0}; {key = 3}; {key = 9}]
    | {key = 5} -> []
    | {key = 6} -> [{key = 0}; {key = 1}; {key = 7}]
    | {key = 7} -> [{key = 2}; {key = 6}]
    | {key = 8} -> [{key = 1}; {key = 3}]
    | {key = 9} -> [{key = 2}; {key = 4}]
    | _ -> exit 1

let rec solve (m : ((int * t), int) Hashtbl.t) (n : int) (k : t) : int =
    match n with
        | 0 -> 1
        | n ->
            let k' : (int * t) = (n, k) in
            match Hashtbl.find_opt m k' with
                | Some v -> v
                | None ->
                    let v : int =
                        move k
                        |> List.map (n - 1 |> solve m)
                        |> List.fold_left (+) 0 in
                    Hashtbl.add m k' v;
                    v

let () : unit =
    let m : ((int * t), int) Hashtbl.t = Hashtbl.create 9 in
    solve m 16 {key = 6} |> Printf.printf "%d\n"
