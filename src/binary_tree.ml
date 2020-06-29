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

    let rec get_first (node : ('a, 'b) t) : ('a, 'b) t =
        match node.left with
            | None -> node
            | Some x -> get_first x

    let rec insert
            (node : ('a, 'b) t)
            (key : 'a)
            (value : 'b)
            (cost : int) : int =
        let cost : int = cost + 1 in
        if Cmp.eq key node.key then
            (
                node.value <- value;
                cost
            )
        else if Cmp.lt key node.key then
            match node.left with
                | Some left -> (insert [@tailcall]) left key value cost
                | None ->
                    (
                        node.left <- Some (make key value);
                        cost
                    )
        else
            match node.right with
                | Some right -> (insert [@tailcall]) right key value cost
                | None ->
                    (
                        node.right <- Some (make key value);
                        cost
                    )

    let rec delete
            (parent : ('a, 'b) t)
            (child : ('a, 'b) t)
            (left : bool)
            (key : 'a) : unit =
        if Cmp.eq key child.key then
            if left then
                match (child.left, child.right) with
                    | (None, None) -> parent.left <- None
                    | (Some _, None) -> parent.left <- child.left
                    | (None, Some _) -> parent.left <- child.right
                    | (Some _, Some right) ->
                        (
                            (get_first right).left <- child.left;
                            parent.left <- child.right
                        )
            else
                match (child.left, child.right) with
                    | (None, None) -> parent.right <- None
                    | (Some _, None) -> parent.right <- child.left
                    | (None, Some _) -> parent.right <- child.right
                    | (Some _, Some right) ->
                        (
                            (get_first right).left <- child.left;
                            parent.right <- child.right
                        )
        else if Cmp.lt key child.key then
            Option.iter
                (fun x -> (delete [@tailcall]) child x true key)
                child.left
        else
            Option.iter
                (fun x -> (delete [@tailcall]) child x false key)
                child.right

    let rec find (node : ('a, 'b) t) (key : 'a) : 'b option =
        if Cmp.eq key node.key then
            Some node.value
        else if Cmp.lt key node.key then
            Option.bind node.left (fun x -> (find [@tailcall]) x key)
        else
            Option.bind node.right (fun x -> (find [@tailcall]) x key)

    let rec traverse (node : ('a, 'b) t) (stack : ('a * 'b) Queue.t) : unit =
        Option.iter (fun x -> (traverse [@tailcall]) x stack) node.left;
        Queue.push (node.key, node.value) stack;
        Option.iter (fun x -> (traverse [@tailcall]) x stack) node.right
end

module Tree (Cmp : Compare) = struct
    module Node = Node (Cmp)

    type ('a, 'b) t = {
        mutable node : ('a, 'b) Node.t option;
        mutable size : int;
    }

    let make () : ('a, 'b) t = {
        node = None;
        size = 0;
    }

    let insert (tree : ('a, 'b) t) (key : 'a) (value : 'b) : int =
        tree.size <- tree.size + 1;
        match tree.node with
            | Some node -> Node.insert node key value 0
            | None ->
                (
                    tree.node <- Some (Node.make key value);
                    0
                )

    let delete (tree : ('a, 'b) t) (key : 'a) : unit =
        tree.size <- tree.size - 1;
        match tree.node with
            | None -> ()
            | Some node ->
                if Cmp.eq key node.Node.key then
                    match (node.Node.left, node.Node.right) with
                        | (None, None) -> tree.node <- None
                        | (Some _, None) -> tree.node <- node.Node.left
                        | (None, Some _) -> tree.node <- node.Node.right
                        | (Some _, Some right) ->
                            (
                                let tip : ('a, 'b) Node.t =
                                    Node.get_first right in
                                tip.Node.left <- node.Node.left;
                                tree.node <- node.Node.right
                            )
                else if Cmp.lt key node.Node.key then
                    Option.iter
                        (fun x -> Node.delete node x true key)
                        node.Node.left
                else
                    Option.iter
                        (fun x -> Node.delete node x false key)
                        node.Node.right

    let find (tree : ('a, 'b) t) (key : 'a) : 'b option =
        Option.bind tree.node (fun node -> Node.find node key)

    let get_queue (tree : ('a, 'b) t) : ('a * 'b) Queue.t =
        let stack : ('a * 'b) Queue.t = Queue.create () in
        Option.iter (fun x -> Node.traverse x stack) tree.node;
        stack
end

module CmpInt = struct
    type t = int
    let eq : (int -> int -> bool) = (=)
    let lt : (int -> int -> bool) = (<)
end

module T = Tree (CmpInt)

let print_insert
        (tree : (int, 'b) T.t)
        (key : int)
        (value : 'b) : unit =
    Printf.printf "insert %2d, cost : %d\n" key (T.insert tree key value)

let print_find (tree : (int, string) T.t) (key : int) : unit =
    match T.find tree key with
        | None -> Printf.printf "find %2d : _\n" key
        | Some value -> Printf.printf "find %2d : \"%s\"\n" key value

let () : unit =
    let tree : (int, string) T.t = T.make () in
    print_insert tree 0 "zero";
    print_insert tree 11 "eleven";
    print_insert tree 4 "four";
    print_insert tree 5 "five";
    print_insert tree 2 "two";
    print_insert tree 10 "ten";
    print_insert tree 1 "one";
    print_insert tree 3 "three";
    print_insert tree 6 "six";
    print_insert tree 12 "twelve";
    print_insert tree 8 "eight";
    print_insert tree 9 "nine";
    print_insert tree 7 "seven";
    Printf.printf "\n";
    print_find tree 2;
    T.delete tree 2;
    T.delete tree 12;
    T.delete tree 0;
    T.delete tree 8;
    T.delete tree 7;
    T.delete tree 11;
    Printf.printf "\n";
    print_find tree 0;
    print_find tree 2;
    print_find tree 7;
    print_find tree 8;
    print_find tree 11;
    print_find tree 12;
    Printf.printf "\n";
    print_find tree 1;
    print_find tree 3;
    print_find tree 4;
    print_find tree 5;
    print_find tree 6;
    print_find tree 9;
    print_find tree 10;
    Printf.printf "\ntree.size : %d\n\n[\n" tree.T.size;
    Queue.iter
        (
            fun (key, value) ->
                let value : string = Printf.sprintf "\"%s\"" value in
                Printf.printf "    (%2d, %8s),\n" key value
        )
        (T.get_queue tree);
    Printf.printf "]\n";
    flush stdout
