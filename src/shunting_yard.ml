type binary_op = Add | Sub | Mul | Div | Pow

type precedence = {
    rank : int;
    left : bool;
}

type t =
    | Num of int
    | BinOp of binary_op
    | UnFn of string
    | BinFn of string
    | LParen
    | RParen

let get_precedence : binary_op -> precedence = function
    | Add | Sub -> { rank = 2; left = true }
    | Mul | Div -> { rank = 3; left = true }
    | Pow -> { rank = 4; left = false }

let show : t -> string = function
    | Num x -> Printf.sprintf "%d" x
    | BinOp Add -> "+"
    | BinOp Sub -> "-"
    | BinOp Mul -> "*"
    | BinOp Div -> "/"
    | BinOp Pow -> "^"
    | UnFn x -> x
    | BinFn x -> x
    | LParen -> "("
    | RParen -> ")"

let valid_operator (a : binary_op) (operator : t Stack.t) : bool =
    match Stack.top_opt operator with
        | None -> false
        | Some LParen -> false
        | Some BinOp b ->
            let a : precedence = get_precedence a in
            let b : precedence = get_precedence b in
            (a.rank < b.rank) || ((a.rank = b.rank) && a.left)
        | Some RParen | Some (Num _) | Some (UnFn _) | Some (BinFn _) ->
            (
                Printf.eprintf "valid_operator\n";
                exit 1
            )

let get_rev_polish (input : t Queue.t) : t Queue.t =
    let output : t Queue.t = Queue.create () in
    let operator : t Stack.t = Stack.create () in
    while Queue.peek_opt input |> Option.is_some do
        match Queue.take input with
            | Num _ as x -> Queue.add x output
            | BinOp x as x' ->
                (
                    while valid_operator x operator do
                        Queue.add (Stack.pop operator) output
                    done;
                    Stack.push x' operator
                )
            | UnFn _ as x -> Stack.push x operator
            | BinFn _ as x -> Stack.push x operator
            | LParen -> Stack.push LParen operator
            | RParen ->
                (
                    while (Stack.top operator) <> LParen do
                        Queue.add (Stack.pop operator) output
                    done;
                    (
                        match Stack.top operator with
                            | LParen -> Stack.pop operator |> ignore
                            | Num _ | RParen | BinOp _ | UnFn _ | BinFn _ -> ()
                    );
                    match Stack.top_opt operator with
                        | Some (UnFn _) | Some (BinFn _) ->
                            Queue.add (Stack.pop operator) output
                        | None | Some (Num _) | Some (BinOp _) | Some LParen
                        | Some RParen -> ()
                )
    done;
    while Stack.top_opt operator |> Option.is_some do
        match Stack.pop operator with
            | BinOp _ as x -> Queue.add x output
            | Num _ | UnFn _ | BinFn _ | LParen | RParen ->
                (
                    Printf.eprintf "get_rev_polish\n";
                    exit 1
                )
    done;
    output

let transform (xs : t array) : string =
    Array.to_seq xs
    |> Queue.of_seq
    |> get_rev_polish
    |> Queue.to_seq
    |> Array.of_seq
    |> Array.map show
    |> Array.to_list
    |> String.concat " "

let () : unit =
    let xs : t array array =
        [|
            [|
                Num 3;
                BinOp Add;
                Num 4;
                BinOp Mul;
                Num 2;
                BinOp Div;
                LParen;
                Num 1;
                BinOp Sub;
                Num 5;
                RParen;
                BinOp Pow;
                Num 2;
                BinOp Pow;
                Num 3;
            |];
            [|
                UnFn "sin";
                LParen;
                BinFn "max";
                LParen;
                Num 2;
                Num 3;
                RParen;
                BinOp Div;
                Num 3;
                BinOp Mul;
                Num 4;
                RParen;
            |];
        |] in
    Array.iter (fun x -> transform x |> (Printf.printf "%s\n")) xs
