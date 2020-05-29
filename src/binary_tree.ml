module Node = struct
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

    let rec insert (node : ('a, 'b) t) (key : 'a) (value : 'b) : unit =
        if key = node.key then
            node.value <- value
        else if key < node.key then
            match node.left with
                | Some left -> (insert [@tailcall]) left key value
                | None -> node.left <- Some (make key value)
        else
            match node.right with
                | Some right -> (insert [@tailcall]) right key value
                | None -> node.right <- Some (make key value)

    let rec find (node : ('a, 'b) t) (key : 'a) : 'b option =
        if key = node.key then
            Some node.value
        else if key < node.key then
            Option.bind node.left (fun left -> (find [@tailcall]) left key)
        else
            Option.bind node.right (fun right -> (find [@tailcall]) right key)
end

module Tree = struct
    type ('a, 'b) t = {
        mutable root : ('a, 'b) Node.t option;
        mutable size : int;
    }

    let make () : ('a, 'b) t = {
        root = None;
        size = 0;
    }

    let insert (tree : ('a, 'b) t) (key : 'a) (value : 'b) : unit =
        tree.size <- tree.size + 1;
        match tree.root with
            | Some node -> Node.insert node key value
            | None -> tree.root <- Some (Node.make key value)

    let find (tree : ('a, 'b) t) (key : 'a) : 'b option =
        Option.bind tree.root (fun node -> Node.find node key)
end

let () : unit =
    let tree : (int, string) Tree.t = Tree.make () in
    Tree.insert tree 0 "zero";
    Tree.insert tree 10 "ten";
    Tree.insert tree 5 "five";
    Tree.insert tree 1 "one";
    Printf.printf "tree.size : %d\n" tree.Tree.size;
    Option.iter (Printf.printf "find  0   : \"%s\"\n") (Tree.find tree 0);
    Option.iter (Printf.printf "find  1   : \"%s\"\n") (Tree.find tree 1);
    Option.iter (Printf.printf "find  5   : \"%s\"\n") (Tree.find tree 5);
    Option.iter (Printf.printf "find 10   : \"%s\"\n") (Tree.find tree 10);
    flush stdout
