let () : unit =
    let xs : int array = [|0; 1; 2; 3; 4; 5; 6; 7; 8; 9|] in
    let n : int = Array.length xs in
    let i : int ref = ref 0 in
    let flag : bool ref = ref false in
    while (not !flag) && (!i < n) do
        Printf.printf "%d\n" xs.(!i);
        if !i = 5 then
            flag := true
        else
            ();
        incr i
    done;
    flush stdout
