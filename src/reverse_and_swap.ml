(* NOTE See `https://codeforces.com/contest/1401/problem/F`. *)

let rec reverse (xs : 'a array) (l : int) (r : int) : unit  =
    if l < r then (
        let t : 'a = xs.(l) in
        xs.(l) <- xs.(r);
        xs.(r) <- t;
        reverse xs (l + 1) (r - 1)
    )

let swap (xs : 'a array) (l1 : int) (r1 : int) (l2 : int) : unit =
    let n : int = r1 - l1 in
    let ts : 'a array = Array.sub xs l1 (n + 1) in
    for i = 0 to n do
        let l2' : int = l2 + i in
        xs.(l1 + i) <- xs.(l2');
        xs.(l2') <- ts.(i)
    done

let sum (xs : int array) (l : int) (r : int) : int =
    let x : int ref = ref 0 in
    for i = l to r do
        x := !x + xs.(i)
    done;
    !x

let print_array (to_string : 'a -> string) (xs : 'a array) : unit =
    Array.map to_string xs
    |> Array.to_list
    |> String.concat ", "
    |> Printf.printf "[%s]\n"

let tokenize (x : string) : (int array * string list) =
    match Str.split (Str.regexp "\n+") x with
        | params::source::queries ->
            let n : int = match Str.split (Str.regexp " +") params with
                | [n; _] -> int_of_string n
                | _ -> exit 1 in
            let buffer : int array = Array.make (2 lsl (n - 1)) 0 in
            (
                Str.split (Str.regexp " +") source
                |> List.iteri (fun i x -> buffer.(i) <- (int_of_string x))
            );
            (buffer, queries)
        | _ -> exit 1

let parse (buffer : int array) (query : string) : unit =
    let n : int = Array.length buffer in
    match Str.split (Str.regexp " +") query |> List.map int_of_string with
        | [1; x; k] -> buffer.(x - 1) <- k
        | [2; k] ->
            let k : int = 2 lsl (k - 1) in
            for i = 1 to n do
                let l : int = ((i - 1) * k) in
                let r : int = ((i * k) - 1) in
                if r < n then
                    reverse buffer l r
            done
        | [3; k] ->
            let k : int = 2 lsl (k - 1) in
            for i = 1 to n do
                let i' : int = 2 * i in
                let l1 : int = (i' - 2) * k in
                let l2 : int = (i' - 1) * k in
                let r1 : int = l2 - 1 in
                let r2 : int = l2 + (r1 - l1) in
                if (l1 < n) && (r1 < n) && (l2 < n) && (r2 < n) then
                    swap buffer l1 r1 l2
            done
        | [4; l; r] -> sum buffer (l - 1) (r - 1) |> Printf.printf "%d\n"
        | _ -> exit 1

let get_stdin () : string =
    let buffer : Buffer.t = Buffer.create 64 in
    (
        try
            while true do
                Buffer.add_string buffer (input_line stdin);
                Buffer.add_char buffer '\n'
            done
        with _ -> ()
    );
    Buffer.contents buffer

let () : unit =
    get_stdin ()
    |> tokenize
    |> (fun (buffer, queries) -> List.iter (parse buffer) queries)
