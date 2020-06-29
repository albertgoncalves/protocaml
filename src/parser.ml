type 'a parser_t = (char list -> ((char list * 'a) list))

let fmap (f : ('a -> 'b)) (p : 'a parser_t) : 'b parser_t =
    fun s -> List.map (fun (s', x) -> (s', f x)) (p s)

let (<$>) (f : ('a -> 'b)) (p : 'a parser_t) : 'b parser_t = fmap f p

let (<*>) (p1 : ('a -> 'b) parser_t) (p2 : 'a parser_t) : 'b parser_t =
    let f ((s', f') : (char list * ('a -> 'b))) : (char list * 'b) list =
        List.map (fun (s'', x) -> (s'', f' x)) (p2 s') in
    fun s -> List.concat_map f (p1 s)

let (|*>) (p1 : 'a parser_t) (p2 : 'b parser_t) : 'b parser_t =
    (fun _ b -> b) <$> p1 <*> p2

let (<*|) (p1 : 'a parser_t) (p2 : 'b parser_t) : 'b parser_t =
    (fun a _ -> a) <$> p1 <*> p2

let (<|>) (p1 : 'a parser_t) (p2 : 'a parser_t) : 'a parser_t =
    fun s -> List.rev_append (p1 s) (p2 s)

let rec at_least_zero (xs : 'a list) (p : 'a parser_t) : 'a list parser_t =
    fun s -> match p s with
        | [] -> [(s, xs)]
        | xs' ->
            List.concat_map (fun (s', x) -> at_least_zero (x :: xs) p s') xs'

let at_least_one (xs : 'a list) (p : 'a parser_t) : 'a list parser_t =
    fun s -> match p s with
        | [] -> []
        | xs' ->
            List.concat_map (fun (s', x) -> at_least_zero (x :: xs) p s') xs'

let satisfy (f : (char -> bool)) : char parser_t = function
    | x :: xs when f x -> [(xs, x)]
    | _ -> []

let is_space : char -> bool = function
    | ' ' | '\012' | '\n' | '\r' | '\t' -> true
    | _ -> false

let any_space : char list parser_t =
    satisfy is_space |> at_least_zero []

let explode (x : string) : char list =
    List.init (String.length x) (String.get x)

let implode (xs : char list) : string =
    let buffer = Buffer.create (List.length xs) in
    List.iter (Buffer.add_char buffer) xs;
    Buffer.contents buffer

let () =
    let (s, x) : (char list * char) =
        explode " \n\t blah" |> (any_space |*> satisfy ((=) 'b')) |> List.hd in
    Printf.printf "(\"%s\", \"%c\")\n%!" (implode s) x
