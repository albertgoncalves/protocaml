type bin_op =
    | AddInt
    | MulInt

type ast =
    | Alloca of string
    | Assign of (string * ast)
    | BinOp of (bin_op * ast * ast)
    | Call1 of (string * ast)
    | LitInt of int
    | LitString of string
    | Var of string

type block =
    {
        instrs : string Queue.t;
        locals : (string, int) Hashtbl.t;
        const_strs : (string, int) Hashtbl.t;
    }

let new_block (const_strs : (string, int) Hashtbl.t) (n : int) : block =
    {
        instrs = Queue.create ();
        locals = Hashtbl.create n;
        const_strs = const_strs;
    }

let register_var (b : block) (s : string) : unit =
    assert (Hashtbl.find_opt b.locals s |> Option.is_none);
    let i : int = Hashtbl.length b.locals in
    Hashtbl.add b.locals s i

let register_const_str (b : block) (s : string) : int =
    match Hashtbl.find_opt b.const_strs s with
        | Some i -> i
        | None ->
            let i : int = Hashtbl.length b.const_strs in
            Hashtbl.add b.const_strs s i;
            i

let push_bin_op (b : block) : bin_op -> unit = function
    | AddInt -> Queue.push "addi" b.instrs
    | MulInt -> Queue.push "muli" b.instrs

let rec push_ast (b : block) : ast -> unit = function
    | Alloca s ->
        register_var b s;
        Queue.push (Printf.sprintf "push _") b.instrs
    | Assign (s, x) ->
        push_ast b x;
        Queue.push
            (Hashtbl.find b.locals s |> Printf.sprintf "store %d")
            b.instrs;
    | Call1 (s, x) ->
        push_ast b x;
        Queue.push (Printf.sprintf "call %s" s) b.instrs;
    | BinOp (op, l, r) ->
        push_ast b l;
        push_ast b r;
        push_bin_op b op;
    | LitInt i -> Queue.push (Printf.sprintf "push %d" i) b.instrs
    | LitString s ->
        Queue.push
            (register_const_str b s |> Printf.sprintf "push %d")
            b.instrs
    | Var s ->
        Queue.push
            (Hashtbl.find b.locals s |> Printf.sprintf "load %d")
            b.instrs

let get_instrs (b : block) : string list =
    b.instrs |> Queue.to_seq |> List.of_seq

let print_instrs (b : block) : unit =
    get_instrs b |> List.iter print_endline

let test_1 () : unit =
    let result : block = new_block (Hashtbl.create 0) 0 in
    (* NOTE: `{ (1 + 1) * (3 + 2); }` *)
    BinOp (
        MulInt,
        BinOp (AddInt, LitInt 1, LitInt 2),
        BinOp (AddInt, LitInt 3, LitInt 4)
    )
    |> push_ast result;
    let expected : string list =
        [
            "push 1";
            "push 2";
            "addi";
            "push 3";
            "push 4";
            "addi";
            "muli";
        ] in
    assert ((get_instrs result) = expected)

let test_2 () : unit =
    let result : block = new_block (Hashtbl.create 0) 2 in
    (* NOTE:
        ```
        {
            i64 x;
            i64 y;
            y = 4;
            x = 1 + 2;
            5 * (x + y);
        }
        ``` *)
    [
        Alloca "x";
        Alloca "y";
        Assign ("y", LitInt 3);
        Assign ("x", BinOp (AddInt, LitInt 1, LitInt 2));
        BinOp (MulInt, LitInt 4, BinOp (AddInt, Var "x", Var "y"));
    ]
    |> List.iter (push_ast result);
    let expected : string list =
        [
            "push _";
            "push _";
            "push 3";
            "store 1";
            "push 1";
            "push 2";
            "addi";
            "store 0";
            "push 4";
            "load 0";
            "load 1";
            "addi";
            "muli";
        ] in
    assert ((get_instrs result) = expected)

let test_3 () : unit =
    let const_strs : (string, int) Hashtbl.t = Hashtbl.create 1 in
    Hashtbl.add const_strs "Hello, world!" 101; (* NOTE: Arbitrary index! *)
    let result : block = new_block const_strs 1 in
    [
        Alloca "x";
        Assign ("x", LitString "Hello, world!");
        Call1 ("print_str_ptr", Var "x");
    ]
    |> List.iter (push_ast result);
    (* NOTE:
        ```
        {
            char* x;
            x = "Hello, world!";
            print(x);
        }
        ``` *)
    let expected : string list =
        [
            "push _";
            "push 101"; (* NOTE: Index into `const_strs` array! *)
            "store 0";
            "load 0";
            "call print_str_ptr";
        ] in
    assert ((get_instrs result) = expected)

let () : unit =
    test_1 ();
    test_2 ();
    test_3 ();
