module StrMap = Map.Make (String)

type bin_op =
    | AddInt
    | MulInt
    | CmpEq

type ast =
    | Alloca of string
    | Assign of (string * ast)
    | BinOp of (bin_op * ast * ast)
    | Break
    | Call1 of (string * ast)
    | Continue
    | If of (ast * ast list)
    | IfElse of (ast * ast list * ast list)
    | LitInt of int
    | LitString of string
    | Loop of ast list
    | Var of string

type const =
    | String of string

type block =
    {
        instrs : string Queue.t;
        mutable locals : int StrMap.t;
        consts : (const, int) Hashtbl.t;
    }

let new_block
        (locals : int StrMap.t)
        (consts : (const, int) Hashtbl.t) : block =
    {
        instrs = Queue.create ();
        locals = locals;
        consts = consts;
    }

let register_var (b : block) (s : string) : int StrMap.t =
    assert (StrMap.find_opt s b.locals |> Option.is_none);
    let i : int = StrMap.cardinal b.locals in
    StrMap.add s i b.locals

let register_const (b : block) (c : const) : int =
    match Hashtbl.find_opt b.consts c with
        | Some i -> i
        | None ->
            (
                let i : int = Hashtbl.length b.consts in
                Hashtbl.add b.consts c i;
                i
            )

let push_bin_op (b : block) : bin_op -> unit = function
    | AddInt -> Queue.push "addi" b.instrs
    | MulInt -> Queue.push "muli" b.instrs
    | CmpEq -> Queue.push "cmpeq" b.instrs

let rec push_ast (b : block) : ast -> unit = function
    | Alloca s ->
        (
            b.locals <- register_var b s;
            Queue.push (Printf.sprintf "push _") b.instrs
        )
    | Assign (s, x) ->
        (
            push_ast b x;
            Queue.push
                (StrMap.find s b.locals |> Printf.sprintf "store %d")
                b.instrs
        )
    | Break -> Queue.push "BREAK" b.instrs
    | Call1 (s, x) ->
        (
            push_ast b x;
            Queue.push (Printf.sprintf "call %s" s) b.instrs
        )
    | Continue -> Queue.push "CONTINUE" b.instrs
    | BinOp (op, l, r) ->
        (
            push_ast b l;
            push_ast b r;
            push_bin_op b op
        )
    | If (x, l) ->
        (
            push_ast b x;
            let child : block = new_block b.locals b.consts in
            List.iter (push_ast child) l;
            Queue.push
                ((Queue.length child.instrs) + 1 |> Printf.sprintf "jpz %d")
                b.instrs;
            Queue.transfer child.instrs b.instrs
        )
    | IfElse (x, l, r) ->
        (
            push_ast b x;
            let child_l : block = new_block b.locals b.consts in
            let child_r : block = new_block b.locals b.consts in
            List.iter (push_ast child_l) l;
            List.iter (push_ast child_r) r;
            let n_r : int = Queue.length child_r.instrs in
            Queue.push (Printf.sprintf "jump %d" (n_r + 1)) child_l.instrs;
            let n_l : int = Queue.length child_l.instrs in
            Queue.push (Printf.sprintf "jpz %d" (n_l + 1)) b.instrs;
            Queue.transfer child_l.instrs b.instrs;
            Queue.transfer child_r.instrs b.instrs
        )
    | LitInt i -> Queue.push (Printf.sprintf "push %d" i) b.instrs
    | LitString s ->
        let i : int = register_const b (String s) in
        Queue.push (Printf.sprintf "push %d" i) b.instrs
    | Loop xs ->
        (
            let child : block = new_block b.locals b.consts in
            List.iter (push_ast child) xs;
            let n : int = Queue.length child.instrs in
            let f (i : int) : string -> unit = function
                | "BREAK" ->
                    Queue.push
                        (Printf.sprintf "jump %d" ((n - i) + 1))
                        b.instrs
                | "CONTINUE" ->
                    Queue.push
                        (Printf.sprintf "jump %d" (-i))
                        b.instrs
                | x -> Queue.push x b.instrs in
            Queue.to_seq child.instrs
            |> List.of_seq
            |> List.iteri f;
            Queue.push (Printf.sprintf "jump %d" (-n)) b.instrs
        )
    | Var s ->
        Queue.push
            (StrMap.find s b.locals |> Printf.sprintf "load %d")
            b.instrs

let get_instrs (b : block) : string list =
    b.instrs |> Queue.to_seq |> List.of_seq

let print_instrs (b : block) : unit =
    get_instrs b |> List.iter print_endline

let test_1 () : unit =
    let result : block = new_block StrMap.empty (Hashtbl.create 0) in
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
    let result : block = new_block StrMap.empty (Hashtbl.create 0) in
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
    let consts : (const, int) Hashtbl.t = Hashtbl.create 1 in
    (* NOTE: Arbitrary index! *)
    Hashtbl.add consts (String "Hello, world!") 101;
    let result : block = new_block StrMap.empty consts in
    (* NOTE:
        ```
        {
            char* x;
            x = "Hello, world!";
            print(x);
        }
        ``` *)
    [
        Alloca "x";
        Assign ("x", LitString "Hello, world!");
        Call1 ("print_str", Var "x");
    ]
    |> List.iter (push_ast result);
    let expected : string list =
        [
            "push _";
            "push 101"; (* NOTE: Index into `consts` array! *)
            "store 0";
            "load 0";
            "call print_str";
        ] in
    assert ((get_instrs result) = expected)

let test_4 () : unit =
    let result : block = new_block StrMap.empty (Hashtbl.create 0) in
    (* NOTE:
        ```
        {
            i64 x;
            x = 1;
            loop {
                print(x)
            }
        }
        ``` *)
    [
        Alloca "x";
        Assign ("x", LitInt 1);
        Loop [Call1 ("print_i64", Var "x")]
    ]
    |> List.iter (push_ast result);
    let expected : string list =
        [
            "push _";
            "push 1";
            "store 0";
            "load 0";
            "call print_i64";
            "jump -2";
        ] in
    assert ((get_instrs result) = expected)

let test_5 () : unit =
    let result : block = new_block StrMap.empty (Hashtbl.create 0) in
    (* NOTE:
        ```
        {
            i64 x;
            i64 y;
            x = 1;
            if x == 2 {
                y = 0
            } else {
                y = 1
            }
        }
        ``` *)
    [
        Alloca "x";
        Alloca "y";
        Assign ("x", LitInt 1);
        IfElse (
            BinOp (CmpEq, Var "x", LitInt 2),
            [Assign ("y", LitInt 0)],
            [Assign ("y", LitInt 1)]
        );
    ]
    |> List.iter (push_ast result);
    let expected : string list =
        [
            "push _";
            "push _";
            "push 1";
            "store 0";
            "load 0";
            "push 2";
            "cmpeq";
            "jpz 4";
            "push 0";
            "store 1";
            "jump 3";
            "push 1";
            "store 1";
        ] in
    assert ((get_instrs result) = expected)

let test_6 () : unit =
    let result : block = new_block StrMap.empty (Hashtbl.create 0) in
    (* NOTE:
        ```
        {
            i64 i;
            i = 0;
            loop {
                if i == 5 {
                    print(i);
                    break;
                }
                i = i + 1;
                if 3 == i {
                    loop {
                        break;
                    }
                    continue;
                }
                print(i);
            }
        }
        ``` *)
    [
        Alloca "i";
        Assign ("i", LitInt 0);
        Loop [
            If (
                BinOp (CmpEq, Var "i", LitInt 5),
                [
                    Call1 ("print_i64", Var "i");
                    Break;
                ]
            );
            Assign ("i", BinOp (AddInt, Var "i", LitInt 1));
            If (
                BinOp (CmpEq, LitInt 3, Var "i"),
                [
                    Loop [
                        Break;
                    ];
                    Continue;
                ]
            );
            Call1 ("print_i64", Var"i");
        ];
    ]
    |> List.iter (push_ast result);
    let expected : string list =
        [
            "push _";
            "push 0";
            "store 0";
            "load 0";
            "push 5";
            "cmpeq";
            "jpz 4";
            "load 0";
            "call print_i64";
            "jump 15";
            "load 0";
            "push 1";
            "addi";
            "store 0";
            "push 3";
            "load 0";
            "cmpeq";
            "jpz 4";
            "jump 2";
            "jump -1";
            "jump -17";
            "load 0";
            "call print_i64";
            "jump -20";
        ] in
    assert ((get_instrs result) = expected)

let () : unit =
    List.iter (fun f -> f ()) [
        test_1;
        test_2;
        test_3;
        test_4;
        test_5;
        test_6;
    ]
