let for_loop (f : (unit -> unit)) (n : int) : unit =
    let m : int = n - 1 in
    for _ = 0 to m do
        f ()
    done

let while_loop (f : (unit -> unit)) (n : int) : unit =
    let i : int ref = ref 0 in
    while !i < n do
        f ();
        incr i
    done

let rec rec_loop (f : (unit -> unit)) : (int -> unit) = function
    | 0 -> ()
    | n ->
        (
            f ();
            rec_loop f (n - 1)
        )

let () : unit =
    let n : int = 10000 in
    let args : (float * float) = (10.1, 20.2) in
    let results = Benchmark.latencyN 10000L [
        (
            "for loop",
            (fun (a, b) -> for_loop (fun () -> ignore (a +. b)) n),
            args
        );
        (
            "while loop",
            (fun (a, b) -> for_loop (fun () -> ignore (a +. b)) n),
            args
        );
        (
            "rec loop",
            (fun (a, b) -> rec_loop (fun () -> ignore (a +. b)) n),
            args
        );
    ] in
    Printf.fprintf stdout "\n";
    Benchmark.tabulate results
