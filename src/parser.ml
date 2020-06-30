type 'a parser_t = (char list -> ((char list * 'a) list))

let fmap (f : ('a -> 'b)) (p : 'a parser_t) : 'b parser_t =
    fun s -> List.map (fun (s', x) -> (s', f x)) (p s)

let (@<$) (a : 'a) (p : 'b parser_t) : 'a parser_t = fmap (fun _ -> a) p

(* NOTE: See `http://blog.shaynefletcher.org/2016/09/custom-operators-in-ocaml.html`. *)
let (@<*>) (p1 : ('a -> 'b) parser_t) (p2 : 'a parser_t) : 'b parser_t =
    let f ((s', f') : (char list * ('a -> 'b))) : (char list * 'b) list =
        List.map (fun (s'', x) -> (s'', f' x)) (p2 s') in
    fun s -> List.concat_map f (p1 s)

let (@<*) (p1 : 'a parser_t) (p2 : 'b parser_t) : 'a parser_t =
    fmap (fun a _ -> a) p1 @<*> p2

let (@*>) (p1 : 'a parser_t) (p2 : 'b parser_t) : 'b parser_t =
    fmap (fun _ b -> b) p1 @<*> p2

let (@<|>) (p1 : 'a parser_t) (p2 : 'a parser_t) : 'a parser_t =
    fun s -> List.rev_append (p1 s) (p2 s)

let rec any (matches : 'a list) (p : 'a parser_t) : 'a list parser_t =
    fun s -> match p s with
        | [] -> [(s, List.rev matches)]
        | xs -> List.concat_map (fun (s', x) -> any (x :: matches) p s') xs

let at_least_one (matches : 'a list) (p : 'a parser_t) : 'a list parser_t =
    fun s -> List.concat_map (fun (s', x) -> any (x :: matches) p s') (p s)

let satisfy (f : (char -> bool)) : char parser_t = function
    | x :: xs when f x -> [(xs, x)]
    | _ -> []

let is_space : char -> bool = function
    | ' ' | '\012' | '\n' | '\r' | '\t' -> true
    | _ -> false

let any_space : char list parser_t = (satisfy is_space |> any [])

let comma : char parser_t = any_space @*> (satisfy ((=) ',')) @<* any_space

let is_digit (x : char) : bool =
    let digit : int = Char.code x in
    ((Char.code '0') <= digit) && (digit <= (Char.code '9'))

let rec match_chars
        (matches : char list)
        (target : char list) : char list parser_t =
    fun s -> match target with
        | [] -> [(s, List.rev matches)]
        | x :: xs ->
            List.concat_map
                (fun (s', x') -> match_chars (x' :: matches) xs s')
                (satisfy ((=) x) s)

let sep_by (sep : 'a parser_t) (item : 'b parser_t) : 'b list parser_t =
    ((fmap (fun a b -> a :: b) item) @<*> (any [] (sep @*> item)))
    @<|> (fun _ -> [])

let explode (x : string) : char list =
    List.init (String.length x) (String.get x)

let implode (xs : char list) : string =
    let buffer = Buffer.create (List.length xs) in
    List.iter (Buffer.add_char buffer) xs;
    Buffer.contents buffer

type json_value =
    | JsonNull
    | JsonBool of bool
    | JsonNumber of int
    | JsonString of string
    | JsonArray of json_value list
    | JsonObject of (string * json_value) list

let json_null : json_value parser_t =
    JsonNull @<$ (match_chars [] (explode "null"))

let json_number : json_value parser_t =
    fmap
        (fun x -> JsonNumber (x |> List.rev |> implode |> int_of_string))
        (at_least_one [] (satisfy is_digit))

let json_string : json_value parser_t =
    fmap
        (fun x -> JsonString (implode x))
        (
            (satisfy ((=) '"'))
            @*> (any [] (satisfy ((<>) '"')))
            @<* (satisfy ((=) '"'))
        )

(* NOTE: OCaml won't let us make the definitions of `json_array` and
   `json_object` recursive along the lines of this implementation. *)
let json_array : json_value parser_t =
    let l_bracket : char parser_t = (satisfy ((=) '[')) @<* any_space in
    let r_bracket : char parser_t = any_space @*> (satisfy ((=) ']')) in
    fmap
        (fun x -> JsonArray x)
        (
            l_bracket
            @*> (sep_by comma (json_null @<|> json_number @<|> json_string))
            @<* r_bracket
        )

let json_object : json_value parser_t =
    let l_brace : char parser_t = (satisfy ((=) '{')) @<* any_space in
    let r_brace : char parser_t = any_space @*> (satisfy ((=) '}')) in
    let string_lit : string parser_t =
        fmap
            (fun x -> List.rev x |> implode)
            (
                (satisfy ((=) '"'))
                @*> (any [] (satisfy ((<>) '"')))
                @<* (satisfy ((=) '"'))
            ) in
    (* NOTE: This is ugly stuff. *)
    let pair : (string * json_value) parser_t =
        (fmap (fun a b -> (a, b)) string_lit)
        @<*>
            (
                any_space
                @*> (satisfy ((=) ':'))
                @*> any_space
                @*>
                    (
                        json_null
                        @<|> json_number
                        @<|> json_string
                        @<|> json_array
                    )
            ) in
    fmap
        (fun x -> JsonObject x)
        (
            l_brace
            @*> (sep_by comma pair)
            @<* r_brace
        )

let any_json : json_value parser_t =
    json_null
    @<|> json_number
    @<|> json_string
    @<|> json_array
    @<|> json_object

let rec json_to_string : json_value -> string = function
    | JsonNull -> "JsonNull"
    | JsonBool b -> Printf.sprintf "JsonBool %B" b
    | JsonNumber n -> Printf.sprintf "JsonNumber %d" n
    | JsonString s -> Printf.sprintf "JsonString %S" s
    | JsonArray xs ->
        List.map json_to_string xs
        |> String.concat ", "
        |> Printf.sprintf "JsonArray [%s]"
    | JsonObject xs ->
        List.map object_to_string xs
        |> String.concat ", "
        |> Printf.sprintf "JsonObject {%s}"

and object_to_string ((k, v) : (string * json_value)) : string =
    Printf.sprintf "%S: %s" k (json_to_string v)

let () : unit =
    explode "{\"foo\": [null, 0, 1, \"bar\"]}"
    |> any_json
    |> List.iter (fun (_, b) -> json_to_string b |> Printf.printf "%s\n%!")
