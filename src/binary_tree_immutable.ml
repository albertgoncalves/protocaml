module type Ord = sig
    type t
    val compare : t -> t -> int
end

module Tree (T : Ord) = struct
    type ('a, 'b) t =
        | Leaf
        | Node of ('a * 'b * ('a, 'b) t * ('a, 'b) t)

    let rec insert (k : 'a) (v : 'b) : ('a, 'b) t -> ('a, 'b) t = function
        | Leaf -> Node (k, v, Leaf, Leaf)
        | Node (k', v', l, r) ->
            let ord : int = T.compare k k' in
            if ord = 0 then
                Node (k, v, l, r)
            else if ord < 0 then
                Node (k', v', (insert k v l), r)
            else
                Node (k', v', l, (insert k v r))

    let rec append (l : ('a, 'b) t) (r : ('a, 'b) t) : ('a, 'b) t =
        match (l, r) with
            | (Leaf, Leaf) -> Leaf
            | (Leaf, _) -> r
            | (_, Leaf) -> l
            | (Node _, Node (k2, v2, l2, r2)) ->
                append (append (insert k2 v2 l) l2) r2

    let rec delete (k : 'a) : ('a, 'b) t -> ('a, 'b) t = function
        | Leaf -> Leaf
        | Node (k', v', l, r) ->
            let ord : int = T.compare k k' in
            if ord = 0 then
                append l r
            else if ord < 0 then
                Node (k', v', (delete k l), r)
            else
                Node (k', v', l, (delete k r))

    let rec lookup (k : 'a) : ('a, 'b) t -> 'b option = function
        | Leaf -> None
        | Node (k', v', l, r) ->
            let ord : int = T.compare k k' in
            if ord = 0 then
                Some v'
            else if ord < 0 then
                lookup k l
            else
                lookup k r

    let pop (k : 'a) (x : ('a, 'b) t) : ('b * ('a, 'b) t) option =
        match lookup k x with
            | None -> None
            | Some v -> Some (v, delete k x)
end

module T = Tree (struct
    type t = int
    let compare : (t -> t -> int) = compare
end)

type t = (int, string) T.t

let test (show : 'a -> string) (a : 'a) (b : 'a) : unit =
    if a = b then
        Printf.eprintf "."
    else (
        Printf.eprintf "!\n\n%s\n\n%s\n%!" (show a) (show b);
        exit 1
    )

let rec pad (i : int) : string =
    if i < 1 then
        ""
    else
        "    " ^ (pad (i - 1))

let show_tree : t -> string =
    let rec loop (i : int) : t -> string = function
        | T.Leaf -> "Leaf"
        | T.Node (k, v, l, r) ->
            let j : int = i + 1 in
            let p0 : string = pad i in
            let p1 : string = pad j in
            Printf.sprintf
                "Node (\n%s%d,\n%s%S,\n%s%s,\n%s%s\n%s)"
                p1
                k
                p1
                v
                p1
                (loop j l)
                p1
                (loop j r)
                p0
    in
    loop 0

let test_insert () : unit =
    let a : t =
        T.Leaf
        |> T.insert 0 "foo bar"
        |> T.insert (-1) "baz qux"
        |> T.insert 2 "quux quuz"
        |> T.insert 1 "corge grault" in
    let b : t =
        T.Node (
            0,
            "foo bar",
            T.Node (-1, "baz qux", T.Leaf, T.Leaf),
            T.Node (
                2,
                "quux quuz",
                T.Node (1, "corge grault", T.Leaf, T.Leaf),
                T.Leaf
            )
        ) in
    test show_tree a b

let test_append () : unit =
    let a : t =
        let l : t = T.Node (
            0,
            "foo bar",
            T.Node (-2, "baz qux", T.Leaf, T.Leaf),
            T.Node (2, "quux quuz", T.Leaf, T.Leaf)
        ) in
        let r : t = T.Node (
            1,
            "corge grault",
            T.Node (-1, "garply waldo", T.Leaf, T.Leaf),
            T.Node (3, "fred plugh", T.Leaf, T.Leaf)
        ) in
        T.append l r in
    let b : t = T.Node (
        0,
        "foo bar",
        T.Node (
            -2,
            "baz qux",
            T.Leaf,
            T.Node (-1, "garply waldo", T.Leaf, T.Leaf)
        ),
        T.Node (
            2,
            "quux quuz",
            T.Node (1, "corge grault", T.Leaf, T.Leaf),
            T.Node (3, "fred plugh", T.Leaf, T.Leaf)
        )
    ) in
    test show_tree a b

let test_delete () : unit =
    let a : t =
        let x : t = T.Node (
            0,
            "foo bar",
            T.Node (
                -2,
                "baz qux",
                T.Leaf,
                T.Node (-1, "garply waldo", T.Leaf, T.Leaf)
            ),
            T.Node (
                2,
                "quux quuz",
                T.Node (1, "corge grault", T.Leaf, T.Leaf),
                T.Node (3, "fred plugh", T.Leaf, T.Leaf)
            )
        ) in
        x |> T.delete 2 |> T.delete (-2) in
    let b : t = T.Node (
        0,
        "foo bar",
        T.Node (-1, "garply waldo", T.Leaf, T.Leaf),
        T.Node (
            1,
            "corge grault",
            T.Leaf,
            T.Node (3, "fred plugh", T.Leaf, T.Leaf)
        )
    ) in
    test show_tree a b

let show_string_option : string option -> string = function
    | None -> "None"
    | Some x -> Printf.sprintf "Some %S" x

let test_lookup_some () : unit =
    let a : string option = Some "corge grault" in
    let b : string option =
        let x : t = T.Node (
            0,
            "foo bar",
            T.Node (
                -2,
                "baz qux",
                T.Leaf,
                T.Node (-1, "garply waldo", T.Leaf, T.Leaf)
            ),
            T.Node (
                2,
                "quux quuz",
                T.Node (1, "corge grault", T.Leaf, T.Leaf),
                T.Node (3, "fred plugh", T.Leaf, T.Leaf)
            )
        ) in
        T.lookup 1 x in
    test show_string_option a b

let test_lookup_none () : unit =
    let x : string option =
        let x : t = T.Node (
            0,
            "foo bar",
            T.Node (
                -2,
                "baz qux",
                T.Leaf,
                T.Node (-1, "garply waldo", T.Leaf, T.Leaf)
            ),
            T.Node (
                2,
                "quux quuz",
                T.Node (1, "corge grault", T.Leaf, T.Leaf),
                T.Node (3, "fred plugh", T.Leaf, T.Leaf)
            )
        ) in
        T.lookup 4 x in
    test show_string_option None x

let show_pop : (string * t) option -> string = function
    | None -> "None"
    | Some (a, b) -> Printf.sprintf "(%S, %s)" a (show_tree b)

let test_pop () : unit =
    let a : (string * t) option =
        let x : t = T.Node (
            0,
            "foo bar",
            T.Node (
                -2,
                "baz qux",
                T.Node (-3, "xyzzy thud", T.Leaf, T.Leaf),
                T.Node (-1, "garply waldo", T.Leaf, T.Leaf)
            ),
            T.Node (
                2,
                "quux quuz",
                T.Node (1, "corge grault", T.Leaf, T.Leaf),
                T.Node (3, "fred plugh", T.Leaf, T.Leaf)
            )
        ) in
        T.pop 0 x in
    let b : (string * t) option =
        let x : t = T.Node (
            -2,
            "baz qux",
            T.Node (-3, "xyzzy thud", T.Leaf, T.Leaf),
            T.Node (
                -1,
                "garply waldo",
                T.Leaf,
                T.Node (
                    2,
                    "quux quuz",
                    T.Node (1, "corge grault", T.Leaf, T.Leaf),
                    T.Node (3, "fred plugh", T.Leaf, T.Leaf)
                )
            )
        ) in
        Some ("foo bar", x) in
    test show_pop a b

let () : unit =
    test_insert ();
    test_append ();
    test_delete ();
    test_lookup_some ();
    test_lookup_none ();
    test_pop ();
    Printf.eprintf "\n%!"
