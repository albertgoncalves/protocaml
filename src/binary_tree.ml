module type Compare = sig
    type t
    val eq : t -> t -> bool
    val lt : t -> t -> bool
end

module Node (Cmp : Compare) = struct
    type ('a, 'b) t = {
        key : 'a;
        mutable value : 'b;
        mutable left : ('a, 'b) t option;
        mutable right : ('a, 'b) t option;
    }

    let make (key : 'a) (value : 'b) : ('a, 'b) t = {
        key;
        value;
        left = None;
        right = None;
    }

    let rec insert
            (node : ('a, 'b) t)
            (key : 'a)
            (value : 'b)
            (cost : int) : int =
        if Cmp.eq key node.key then
            (
                node.value <- value;
                cost + 1
            )
        else if Cmp.lt key node.key then
            match node.left with
                | Some left -> (insert [@tailcall]) left key value (cost + 1)
                | None ->
                    (
                        node.left <- Some (make key value);
                        cost + 1
                    )
        else
            match node.right with
                | Some right -> (insert [@tailcall]) right key value (cost + 1)
                | None ->
                    (
                        node.right <- Some (make key value);
                        cost +1
                    )

    let rec find (node : ('a, 'b) t) (key : 'a) : 'b option =
        if Cmp.eq key node.key then
            Some node.value
        else if Cmp.lt key node.key then
            Option.bind node.left (fun left -> (find [@tailcall]) left key)
        else
            Option.bind node.right (fun right -> (find [@tailcall]) right key)
end

module Tree (Cmp : Compare) = struct
    module Node = Node (Cmp)

    type ('a, 'b) t = {
        mutable root : ('a, 'b) Node.t option;
        mutable size : int;
    }

    let make () : ('a, 'b) t = {
        root = None;
        size = 0;
    }

    let insert (tree : ('a, 'b) t) (key : 'a) (value : 'b) : int =
        tree.size <- tree.size + 1;
        match tree.root with
            | Some node -> Node.insert node key value 0
            | None ->
                (
                    tree.root <- Some (Node.make key value);
                    0
                )

    let find (tree : ('a, 'b) t) (key : 'a) : 'b option =
        Option.bind tree.root (fun node -> Node.find node key)
end

module CmpInt = struct
    type t = int
    let eq : (int -> int -> bool) = (=)
    let lt : (int -> int -> bool) = (<)
end

module T = Tree (CmpInt)

let () : unit =
    let tree : (int, string) T.t = T.make () in
    Printf.printf "insert  2, cost : %d\n" (T.insert tree 2 "two");
    Printf.printf "insert 10, cost : %d\n" (T.insert tree 10 "ten");
    Printf.printf "insert  5, cost : %d\n" (T.insert tree 5 "five");
    Printf.printf "insert  1, cost : %d\n" (T.insert tree 1 "one");
    Printf.printf "insert  0, cost : %d\n" (T.insert tree 0 "zero");
    Printf.printf "insert  4, cost : %d\n" (T.insert tree 4 "four");
    Printf.printf "insert  6, cost : %d\n" (T.insert tree 6 "six");
    Printf.printf "insert  3, cost : %d\n" (T.insert tree 3 "three");
    Printf.printf "insert 11, cost : %d\n" (T.insert tree 11 "eleven");
    Printf.printf "\ntree.size : %d\n" tree.T.size;
    Option.iter (Printf.printf "\nfind  0   : \"%s\"\n") (T.find tree 0);
    Option.iter (Printf.printf "find  1   : \"%s\"\n") (T.find tree 1);
    Option.iter (Printf.printf "find  5   : \"%s\"\n") (T.find tree 5);
    Option.iter (Printf.printf "find 10   : \"%s\"\n") (T.find tree 10);
    flush stdout
