let rec distance
        (memo : ((char list * char list), int) Hashtbl.t)
        (a : char list)
        (b : char list) : int =
    match Hashtbl.find_opt memo (a, b) with
        | Some x -> x
        | None ->
            let n : int = List.length a in
            let m : int = List.length b in
            match (a, b) with
                | ([], _) ->
                    Hashtbl.add memo (a, b) m;
                    m
                | (_, []) ->
                    Hashtbl.add memo (a, b) n;
                    n
                | ((a :: as' as as''), (b :: bs' as bs'')) ->
                    if a == b then (
                        let x : int = distance memo as' bs' in
                        Hashtbl.add memo (as'', bs'') x;
                        x
                    ) else (
                        let i : int = distance memo as'' bs' in
                        let j : int = distance memo as' bs'' in
                        let k : int = distance memo as' bs' in
                        let x : int = 1 + (min i (min j k)) in
                        Hashtbl.add memo (as'', bs'') x;
                        x
                    )

let explode (x : string) : char list =
    List.init (String.length x) (String.get x)

let () : unit =
    let test (a : string) (b : string) : int -> bool =
        let memo : ((char list * char list), int) Hashtbl.t =
            Hashtbl.create 64 in
        (=) (distance memo (explode a) (explode b)) in
    let print : bool -> unit = Printf.printf "%b\n" in
    print (test "foobar" "" 6);
    print (test "sitting" "kitten" 3);
    print (test "flaw" "lawn" 2);
    print (test "saturday" "sunday" 3);
    print (test "gumbo" "gambol" 2);
    print (test "book" "back" 2);
    print (test "edward" "edwin" 3);
    print (
        test
            "the quick brown fox jumps over the lazy dog"
            "pack my box with five dozen liquor jugs"
            33
    )
