(* NOTE: See `https://arxiv.org/abs/cs/0603080v1`. *)

type type_t =
    | Var of char
    | Term of char

type expr_t =
    | Atom of type_t
    | Func of (char * expr_t list)

let string_to_chars (s : string) : char list =
    List.init (String.length s) (String.get s)

let is_alpha_upper (c : int) : bool =
    ((Char.code 'A') <= c) && (c <= (Char.code 'Z'))

let is_alpha_lower (c : int) : bool =
    ((Char.code 'a') <= c) && (c <= (Char.code 'z'))

let rec skip_space : char list -> char list = function
    | ' ' :: cs -> skip_space cs
    | cs -> cs

let rec get_func (cs : char list) : (expr_t * char list) =
    let (c, cs') : (char * char list) =
        match skip_space cs with
            | c :: cs' when Char.code c |> is_alpha_lower -> (c, cs')
            | _ -> failwith "get_func" in
    let rec f (exprs : expr_t list) : char list -> (expr_t * char list) =
        function
            | [] -> failwith "get_func"
            | ')' :: cs'' -> (Func (c, List.rev exprs), skip_space cs'')
            | cs'' ->
                let (expr, cs''') : (expr_t * char list) = get_expr cs'' in
                f (expr :: exprs) cs''' in
    f [] (skip_space cs')

and get_expr (cs : char list) : (expr_t * char list) =
    match skip_space cs with
        | [] -> failwith "get_expr"
        | '(' :: cs' -> get_func cs'
        | c :: cs' ->
            match Char.code c with
                | c' when is_alpha_upper c' -> (Atom (Var c), skip_space cs')
                | c' when is_alpha_lower c' -> (Atom (Term c), skip_space cs')
                | _ -> failwith "get_expr"

let parse (s : string) : expr_t =
    match string_to_chars s |> get_expr with
        | (expr, []) -> expr
        | _ -> failwith "parse"

let () : unit =
    let output : expr_t = parse " ( f A b ) " in
    let expected : expr_t = Func ('f', [Atom (Var 'A'); Atom (Term 'b')]) in
    assert (output = expected)
