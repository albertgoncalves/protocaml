let rec distance
        (memo : ((char list * char list), int) Hashtbl.t)
        (ls : char list)
        (rs : char list) : int =
    match Hashtbl.find_opt memo (ls, rs) with
        | Some x -> x
        | None ->
            let n : int = List.length ls in
            let m : int = List.length rs in
            match (ls, rs) with
                | ([], _) ->
                    Hashtbl.add memo (ls, rs) m;
                    m
                | (_, []) ->
                    Hashtbl.add memo (ls, rs) n;
                    n
                | ((l :: ls as ls'), (r :: rs as rs')) ->
                    if l = r then (
                        let x : int = distance memo ls rs in
                        Hashtbl.add memo (ls', rs') x;
                        x
                    ) else (
                        let i : int = distance memo ls' rs in
                        let j : int = distance memo ls rs' in
                        let k : int = distance memo ls rs in
                        let x : int = 1 + (min i (min j k)) in
                        Hashtbl.add memo (ls', rs') x;
                        x
                    )

let explode (x : string) : char list =
    List.init (String.length x) (String.get x)

let () : unit =
    let test (a : string) (b : string) : int -> bool =
        let memo : ((char list * char list), int) Hashtbl.t =
            Hashtbl.create 64 in
        (=) (distance memo (explode a) (explode b)) in
    Array.iter
        (Printf.printf "%B\n")
        [|
            test "foobar" "" 6;
            test "sitting" "kitten" 3;
            test "flaw" "lawn" 2;
            test "saturday" "sunday" 3;
            test "gumbo" "gambol" 2;
            test "book" "back" 2;
            test "edward" "edwin" 3;
            test
                "the quick brown fox jumps over the lazy dog"
                "pack my box with five dozen liquor jugs"
                33;
        |]
