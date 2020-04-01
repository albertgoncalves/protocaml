let for_loop (f : (unit -> unit)) (n : int) : unit =
    for _ = 0 to n do
        f ()
    done

let while_loop (f : (unit -> unit)) (n : int) : unit =
    let i : int ref = ref 0 in
    while !i <= n do
        f ();
        incr i
    done

let rec rec_loop (f : (unit -> unit)) : (int -> unit) = function
    | 0 -> f ()
    | n ->
        (
            f ();
            rec_loop f (n - 1)
        )

let () : unit =
    let n : int = 5000 in
    let args : (float * float) = (10.1, 20.2) in
    let res = Benchmark.latencyN (20000L : int64) [
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
    print_newline();
    Benchmark.tabulate res
