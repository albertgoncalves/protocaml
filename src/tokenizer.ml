type token =
    | LParen
    | RParen
    | Ident of string
    | Num of int
    | Str of string

type tokenizer = {
    chars : string;
    mutable len : int;
    mutable flag : bool;
}

let get_int (s : string) : int option =
    let zero : int = Char.code '0' in
    let rec f (i : int) (decimal : int) (result : int) : int option =
        if 0 <= i then
            let digit : int = Char.code s.[i] - zero in
            if (0 <= digit) && (digit <= 9) then
                f (i - 1) (decimal * 10) (result + (digit * decimal))
            else
                None
        else
            Some result in
    f ((String.length s) - 1) 1 0

let push_lit (t : tokenizer) (ts : token list ref) (i : int) : unit =
    if 0 < t.len then
        let s : string = String.sub t.chars (i - t.len) t.len in
        match get_int s with
            | Some n -> ts := (Num n) :: !ts
            | None ->
                (
                    ts := (Ident s) :: !ts;
                    t.len <- 0
                )

let push_str (t : tokenizer) (ts : token list ref) (i : int) : unit =
    if 0 < t.len then
        (
            ts := (Str (String.sub t.chars (i - t.len) t.len)) :: !ts;
            t.len <- 0
        )

let push_token (t : tokenizer) (ts : token list ref) (i : int) : char -> unit =
    if t.flag then function
        | '"' ->
            (
                t.flag <- false;
                push_str t ts i;
                t.len <- 0
            )
        | _ -> t.len <- t.len + 1
    else function
        | '(' ->
            (
                push_lit t ts i;
                ts := LParen :: !ts;
                t.len <- 0
            )
        | ')' ->
            (
                push_lit t ts i;
                ts := RParen :: !ts;
                t.len <- 0
            )
        | ' ' ->
            (
                push_lit t ts i;
                t.len <- 0
            )
        | '"' ->
            (
                t.flag <- true;
                t.len <- 0
            )
        | _ -> t.len <- t.len + 1

let tokenizer (s : string) : token list =
    let t : tokenizer = {chars = s; len = 0; flag = false} in
    let ts : token list ref = ref [] in
    String.iteri (push_token t ts) s;
    !ts |> List.rev

let token_to_string : token -> string = function
    | LParen -> "LParen"
    | RParen -> "RParen"
    | Ident x -> Printf.sprintf "Ident %S" x
    | Str x -> Printf.sprintf "Str %S" x
    | Num x -> Printf.sprintf "Num %d" x

let () : unit =
    tokenizer "(f (g 1234567890 0987654321) \"foo bar\" x)"
    |> List.map token_to_string
    |> List.iter (Printf.printf "%s\n")
