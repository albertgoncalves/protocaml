(* NOTE: See `https://github.com/ocaml/ocaml/blob/trunk/stdlib/queue.ml`. *)
module Queue' = struct
    type 'a cell =
        | Nil
        | Cons of
              {
                  content : 'a;
                  mutable next : 'a cell
              }

    type 'a t =
        {
            mutable length : int;
            mutable first : 'a cell;
            mutable last : 'a cell
        }

    let create () : 'a t =
        {
            length = 0;
            first = Nil;
            last = Nil;
        }

    let clear (q : 'a t) : unit =
        q.length <- 0;
        q.first <- Nil;
        q.last <- Nil

    let push (x : 'a) (q : 'a t) : unit =
        let cell : 'a cell = Cons {
            content = x;
            next = Nil
        } in
        match q.last with
            | Nil ->
                (
                    q.length <- 1;
                    q.first <- cell;
                    q.last <- cell
                )
            | Cons last ->
                (
                    q.length <- q.length + 1;
                    last.next <- cell;
                    q.last <- cell
                )

    let iter : ('a -> unit) -> 'a t -> unit =
        let rec iter (f : 'a -> unit) (cell : 'a cell) : unit =
            match cell with
                | Nil -> ()
                | Cons { content; next } ->
                    (
                        f content;
                        iter f next
                    ) in
        fun f q -> iter f q.first

    let iteri : (int -> 'a -> unit) -> 'a t -> unit =
        let rec iteri
                (f : int -> 'a -> unit)
                (i : int)
                (cell : 'a cell) : unit =
            match cell with
                | Nil -> ()
                | Cons { content; next } ->
                    (
                        f i content;
                        iteri f (i + 1) next
                    ) in
        fun f q -> iteri f 0 q.first

    let transfer (q1 : 'a t) (q2 : 'a t) : unit =
        if 0 < q1.length then
            match q2.last with
                | Nil ->
                    (
                        q2.length <- q1.length;
                        q2.first <- q1.first;
                        q2.last <- q1.last;
                        clear q1
                    )
                | Cons last ->
                    (
                        q2.length <- q2.length + q1.length;
                        last.next <- q1.first;
                        q2.last <- q1.last;
                        clear q1
                    )

    let fold : ('b -> 'a -> 'b) -> 'b -> 'a t -> 'b =
        let rec fold (f : 'b -> 'a -> 'b) (accu : 'b) : 'a cell -> 'b =
            function
                | Nil -> accu
                | Cons { content; next } -> fold f (f accu content) next in
        fun f accu q -> fold f accu q.first

    let to_seq (q : 'a t) : 'a Seq.t =
        let rec f (c : 'a cell) () : 'a Seq.node =
            match c with
                | Nil -> Seq.Nil
                | Cons { content = x; next } -> Seq.Cons (x, f next) in
        f q.first
end

module StrMap = Map.Make (String)

type typ =
    | I64
    | U64
    | Char
    | Ptr of typ

type arg = (string * typ)

type bin_op =
    | AddI64
    | MulI64
    | Eq

type ast =
    | Alloca of (string * typ)
    | Assign of (string * ast)
    | BinOp of (bin_op * ast * ast)
    | Break
    | Call of (string * ast list)
    | Continue
    | Fn of (string * arg list * typ list * ast list)
    | If of (ast * ast list)
    | IfElse of (ast * ast list * ast list)
    | LitInt of int
    | LitString of string
    | Loop of ast list
    | Return of ast list
    | Var of string

type const =
    | String of string

type block =
    {
        instrs : string Queue'.t;
        mutable locals : int StrMap.t;
        consts : (const, int) Hashtbl.t;
        fns : (string, int) Hashtbl.t;
    }

let new_block
        (locals : int StrMap.t)
        (consts : (const, int) Hashtbl.t)
        (fns : (string, int) Hashtbl.t) : block =
    {
        instrs = Queue'.create ();
        locals = locals;
        consts = consts;
        fns = fns;
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

let pad_returns (b : block) : int =
    (* NOTE: A `RETURN` statement will *always* unpack into 5 instrs;
       let's add 4 to all such statements we haven't yet unpacked. *)
    let f (n : int) : string -> int = function
        | "RETURN" -> n + 1
        | _ -> n in
    4 * (Queue'.fold f 0 b.instrs)

let register_fn (b : block) (s : string) : unit =
    assert (Hashtbl.find_opt b.fns s |> Option.is_none);
    let i : int = Hashtbl.length b.fns in
    Hashtbl.add b.fns s i

let push_bin_op (b : block) : bin_op -> unit = function
    | AddI64 -> Queue'.push "addi" b.instrs
    | MulI64 -> Queue'.push "muli" b.instrs
    | Eq -> Queue'.push "eq" b.instrs

let rec push_ast (b : block) : ast -> unit = function
    | Alloca (s, _) ->
        (
            b.locals <- register_var b s;
            Queue'.push (Printf.sprintf "rsrv 1") b.instrs
        )
    | Assign (s, x) ->
        (
            push_ast b x;
            Queue'.push
                (StrMap.find s b.locals |> Printf.sprintf "store %d")
                b.instrs
        )
    | Break -> Queue'.push "BREAK" b.instrs
    | Call (s, xs) ->
        (
            List.iter (push_ast b) xs;
            Queue'.push
                (
                    (* NOTE: A `string` is not a valid argument; eventually
                       this will need to be revised. *)
                    match Hashtbl.find_opt b.fns s with
                        | None -> Printf.sprintf "call %s" s
                        | Some i -> Printf.sprintf "call %d" i
                )
                b.instrs
        )
    | Continue -> Queue'.push "CONTINUE" b.instrs
    | Fn (s, args, rets, xs) ->
        (
            (* NOTE: This is not the best way to map `function` labels to their
               respective instruction indices. *)
            register_fn b s;
            let child : block = new_block StrMap.empty b.consts b.fns in
            let rec f : arg list -> unit = function
                | [] -> ()
                | (s, _) :: xs ->
                    (
                        child.locals <- register_var child s;
                        f xs
                    ) in
            (* NOTE: We need to save some data to the stack to be able to
               return from the function routine; basically need to carve out
               space for two additional `local` values. *)
            f ((".ip", U64) :: (".bp", U64) :: args);
            Queue'.push "save" child.instrs;
            let n_args : int = List.length args in
            Queue'.push (n_args + 2 |> Printf.sprintf "frame %d") child.instrs;
            if n_args <> 0 then (
                let bury : string = n_args + 1 |> Printf.sprintf "bury %d" in
                Queue'.push bury child.instrs;
                Queue'.push bury child.instrs
            );
            List.iter (push_ast child) xs;
            let n_locals : int = StrMap.cardinal child.locals in
            let n_rets : int = List.length rets in
            let f : string -> unit = function
                | "RETURN" ->
                    (
                        for _ = 0 to (n_rets - 1) do
                            Queue'.push
                                (
                                    n_locals + (n_rets - 1)
                                    |> Printf.sprintf "bury %d"
                                )
                                b.instrs
                        done;
                        if 2 < n_locals then (
                            Queue'.push
                                (n_locals - 2 |> Printf.sprintf "drop %d")
                                b.instrs;
                        );
                        Queue'.push "reset" b.instrs;
                        Queue'.push "ret" b.instrs
                    )
                | x -> Queue'.push x b.instrs in
            Queue'.iter f child.instrs;
        )
    | BinOp (op, l, r) ->
        (
            push_ast b l;
            push_ast b r;
            push_bin_op b op
        )
    | If (x, l) ->
        (
            push_ast b x;
            let child : block = new_block b.locals b.consts b.fns in
            List.iter (push_ast child) l;
            let n : int =
                (child.instrs.Queue'.length + 1) + (pad_returns child) in
            Queue'.push (Printf.sprintf "jpz %d" n) b.instrs;
            Queue'.transfer child.instrs b.instrs
        )
    | IfElse (x, l, r) ->
        (
            push_ast b x;
            let child_l : block = new_block b.locals b.consts b.fns in
            let child_r : block = new_block b.locals b.consts b.fns in
            List.iter (push_ast child_l) l;
            List.iter (push_ast child_r) r;
            let n_r : int =
                (child_r.instrs.Queue'.length + 1) + (pad_returns child_r) in
            Queue'.push (Printf.sprintf "jump %d" n_r) child_l.instrs;
            let n_l : int =
                (child_l.instrs.Queue'.length + 1) + (pad_returns child_l) in
            Queue'.push (Printf.sprintf "jpz %d" n_l) b.instrs;
            Queue'.transfer child_l.instrs b.instrs;
            Queue'.transfer child_r.instrs b.instrs
        )
    | LitInt i -> Queue'.push (Printf.sprintf "push %d" i) b.instrs
    | LitString s ->
        let i : int = register_const b (String s) in
        Queue'.push (Printf.sprintf "push %d" i) b.instrs
    | Loop xs ->
        (
            let child : block = new_block b.locals b.consts b.fns in
            List.iter (push_ast child) xs;
            let n : int =  child.instrs.Queue'.length in
            let f (i : int) : string -> unit = function
                | "BREAK" ->
                    Queue'.push
                        (Printf.sprintf "jump %d" ((n - i) + 1))
                        b.instrs
                | "CONTINUE" ->
                    Queue'.push
                        (Printf.sprintf "jump %d" (-i))
                        b.instrs
                | x -> Queue'.push x b.instrs in
            Queue'.iteri f child.instrs;
            Queue'.push (Printf.sprintf "jump %d" (-n)) b.instrs
        )
    | Return xs ->
        (
            List.iter (push_ast b) xs;
            Queue'.push "RETURN" b.instrs
        )
    | Var s ->
        Queue'.push
            (StrMap.find s b.locals |> Printf.sprintf "copy %d")
            b.instrs

let get_instrs (b : block) : string list =
    b.instrs |> Queue'.to_seq |> List.of_seq

let print_instrs (b : block) : unit =
    get_instrs b |> List.iter print_endline

let test_1 () : unit =
    let result : block =
        new_block StrMap.empty (Hashtbl.create 0) (Hashtbl.create 0) in
    (* NOTE: `{ (1 + 1) * (3 + 2); }` *)
    BinOp (
        MulI64,
        BinOp (AddI64, LitInt 1, LitInt 2),
        BinOp (AddI64, LitInt 3, LitInt 4)
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
    let result : block =
        new_block StrMap.empty (Hashtbl.create 0) (Hashtbl.create 0) in
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
        Alloca ("x", I64);
        Alloca ("y", I64);
        Assign ("y", LitInt 3);
        Assign ("x", BinOp (AddI64, LitInt 1, LitInt 2));
        BinOp (MulI64, LitInt 4, BinOp (AddI64, Var "x", Var "y"));
    ]
    |> List.iter (push_ast result);
    let expected : string list =
        [
            "rsrv 1";
            "rsrv 1";
            "push 3";
            "store 1";
            "push 1";
            "push 2";
            "addi";
            "store 0";
            "push 4";
            "copy 0";
            "copy 1";
            "addi";
            "muli";
        ] in
    assert ((get_instrs result) = expected)

let test_3 () : unit =
    let consts : (const, int) Hashtbl.t = Hashtbl.create 1 in
    (* NOTE: Arbitrary index! *)
    Hashtbl.add consts (String "Hello, world!") 101;
    let result : block = new_block StrMap.empty consts (Hashtbl.create 0) in
    (* NOTE:
        ```
        {
            char* x;
            x = "Hello, world!";
            print(x);
        }
        ``` *)
    [
        Alloca ("x", Ptr Char);
        Assign ("x", LitString "Hello, world!");
        Call ("print_str", [Var "x"]);
    ]
    |> List.iter (push_ast result);
    let expected : string list =
        [
            "rsrv 1";
            "push 101"; (* NOTE: Index into `consts` array! *)
            "store 0";
            "copy 0";
            "call print_str";
        ] in
    assert ((get_instrs result) = expected)

let test_4 () : unit =
    let result : block =
        new_block StrMap.empty (Hashtbl.create 0) (Hashtbl.create 0) in
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
        Alloca ("x", I64);
        Assign ("x", LitInt 1);
        Loop [Call ("print_i64", [Var "x"])]
    ]
    |> List.iter (push_ast result);
    let expected : string list =
        [
            "rsrv 1";
            "push 1";
            "store 0";
            "copy 0";
            "call print_i64";
            "jump -2";
        ] in
    assert ((get_instrs result) = expected)

let test_5 () : unit =
    let result : block =
        new_block StrMap.empty (Hashtbl.create 0) (Hashtbl.create 0) in
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
        Alloca ("x", I64);
        Alloca ("y", I64);
        Assign ("x", LitInt 1);
        IfElse (
            BinOp (Eq, Var "x", LitInt 2),
            [Assign ("y", LitInt 0)],
            [Assign ("y", LitInt 1)]
        );
    ]
    |> List.iter (push_ast result);
    let expected : string list =
        [
            "rsrv 1";
            "rsrv 1";
            "push 1";
            "store 0";
            "copy 0";
            "push 2";
            "eq";
            "jpz 4";
            "push 0";
            "store 1";
            "jump 3";
            "push 1";
            "store 1";
        ] in
    assert ((get_instrs result) = expected)

let test_6 () : unit =
    let result : block =
        new_block StrMap.empty (Hashtbl.create 0) (Hashtbl.create 0) in
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
        Alloca ("i", I64);
        Assign ("i", LitInt 0);
        Loop [
            If (
                BinOp (Eq, Var "i", LitInt 5),
                [
                    Call ("print_i64", [Var "i"]);
                    Break;
                ]
            );
            Assign ("i", BinOp (AddI64, Var "i", LitInt 1));
            If (
                BinOp (Eq, LitInt 3, Var "i"),
                [
                    Loop [
                        Break;
                    ];
                    Continue;
                ]
            );
            Call ("print_i64", [Var "i"]);
        ];
    ]
    |> List.iter (push_ast result);
    let expected : string list =
        [
            "rsrv 1";
            "push 0";
            "store 0";
            "copy 0";
            "push 5";
            "eq";
            "jpz 4";
            "copy 0";
            "call print_i64";
            "jump 15";
            "copy 0";
            "push 1";
            "addi";
            "store 0";
            "push 3";
            "copy 0";
            "eq";
            "jpz 4";
            "jump 2";
            "jump -1";
            "jump -17";
            "copy 0";
            "call print_i64";
            "jump -20";
        ] in
    assert ((get_instrs result) = expected)

let test_7 () : unit =
    let result : block =
        new_block StrMap.empty (Hashtbl.create 0) (Hashtbl.create 0) in
    (* NOTE:
        ```
        i64 f(i64 x, i64 y) {
            i64 z;
            z = x + y;
            return z;
        }

        void g() {
            print(f(4, 5));
        }
        ``` *)
    [
        Fn (
            "f",
            [("x", I64); ("y", I64)],
            [I64],
            [
                Alloca ("z", I64);
                Assign ("z", BinOp (AddI64, Var "x", Var "y"));
                Return [Var "z"];
            ]
        );
        Fn (
            "g",
            [],
            [],
            [
                Call ("print_i64", [Call ("f", [LitInt 4; LitInt 5])]);
                Return [];
            ]
        );
    ]
    |> List.iter (push_ast result);
    let expected : string list =
        [
            "save";
            "frame 4";
            "bury 3";
            "bury 3";
            "rsrv 1";
            "copy 2";
            "copy 3";
            "addi";
            "store 4";
            "copy 4";
            "bury 5";
            "drop 3";
            "reset";
            "ret";
            "save";
            "frame 2";
            "push 4";
            "push 5";
            "call 0";
            "call print_i64";
            "reset";
            "ret";
        ] in
    assert ((get_instrs result) = expected)

let test_8 () : unit =
    let result : block =
        new_block StrMap.empty (Hashtbl.create 0) (Hashtbl.create 0) in
    (* NOTE:
        ```
        (i64, i64) f(i64 x, i64 y, i64 z) {
            i64 w;
            w = (x + y) + z;
            if x == 0 {
                return (x, y);
            } else {
                if 0 == y {
                    return (y, x + z);
                }
            }
            return (1 + x, w);
        }
        ``` *)
    [
        Fn (
            "f",
            [("x", I64); ("y", I64); ("z", I64)],
            [I64; I64],
            [
                Alloca ("w", I64);
                Assign (
                    "w",
                    BinOp (AddI64, BinOp (AddI64, Var "x", Var "y"), Var "z")
                );
                IfElse (
                    BinOp (Eq, Var "x", LitInt 0),
                    [Return [Var "x"; Var "y"]],
                    [
                        If (
                            BinOp (Eq, LitInt 0, Var "y"),
                            [
                                Return [
                                    Var "y";
                                    BinOp (AddI64, Var "x", Var "z");
                                ];
                            ]
                        );
                    ]
                );
                Return [BinOp (AddI64, LitInt 1, Var "x"); Var "w"];
            ]
        );
    ]
    |> List.iter (push_ast result);
    let expected : string list =
        [
            "save";
            "frame 5";
            "bury 4";
            "bury 4";
            "rsrv 1";
            "copy 2";
            "copy 3";
            "addi";
            "copy 4";
            "addi";
            "store 5";
            "copy 2";
            "push 0";
            "eq";
            "jpz 9";
            "copy 2";
            "copy 3";
            "bury 7";
            "bury 7";
            "drop 4";
            "reset";
            "ret";
            "jump 14";
            "push 0";
            "copy 3";
            "eq";
            "jpz 10";
            "copy 3";
            "copy 2";
            "copy 4";
            "addi";
            "bury 7";
            "bury 7";
            "drop 4";
            "reset";
            "ret";
            "push 1";
            "copy 2";
            "addi";
            "copy 5";
            "bury 7";
            "bury 7";
            "drop 4";
            "reset";
            "ret";
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
        test_7;
        test_8;
    ]
