type expr =
  | Int of int
  | Ident of string
  | Call of expr * (expr list)
  | Func of (string list) * (stmt list)
  | Array of expr list

and stmt =
  | Let of string * expr
  | Set of expr * expr
  | Expr of expr
  | Ret of (expr option)

type state_idents = {
  global : (string, unit) Hashtbl.t;
  local : (string, unit) Hashtbl.t;
  escape : (string, int) Hashtbl.t;
  heap : (string, unit) Hashtbl.t;
}

type state_stmts = {
  global : stmt Queue.t;
  local : stmt Queue.t;
}

type state = {
  stmts : state_stmts;
  idents : state_idents;
  mutable k : int;
}

let rec show_expr indent =
  function
  | Int int -> string_of_int int
  | Ident ident -> ident

  | Call (Ident "[", [array; index]) -> show_expr indent array ^ "[" ^ show_expr indent index ^ "]"
  | Call (Ident "[", _) -> assert false

  | Call (Ident "+", [left; right]) ->
    "(" ^ show_expr indent left ^ " + " ^ show_expr indent right ^ ")"
  | Call (Ident "+", _) -> assert false

  | Call (func, args) ->
    show_expr indent func ^ "(" ^ String.concat ", " (List.map (show_expr indent) args) ^ ")"

  | Func (args, stmts) ->
    "(function("
    ^ String.concat ", " args
    ^ ") {\n"
    ^ String.concat
      ""
      (List.map (fun stmt -> String.make (indent + 4) ' ' ^ show_stmt (indent + 4) stmt ^ "\n") stmts)
    ^ String.make indent ' '
    ^ "})"

  | Array exprs -> "[" ^ String.concat ", " (List.map (show_expr indent) exprs) ^ "]"

and show_stmt indent =
  function
  | Let (ident, expr) -> "var " ^ ident ^ " = " ^ show_expr indent expr ^ ";"
  | Set (expr_to, expr_from) -> show_expr indent expr_to ^ " = " ^ show_expr indent expr_from ^ ";"
  | Expr expr -> show_expr indent expr ^ ";"
  | Ret (Some expr) -> "return " ^ show_expr indent expr ^ ";"
  | Ret None -> "return;"

let add_with_index table key =
  match Hashtbl.find_opt table key with
  | Some index -> index
  | None ->
    (
      let index = Hashtbl.length table in
      Hashtbl.add table key index;
      index
    )

let generate_ident state =
  let k = state.k in
  state.k <- k + 1;
  "__" ^ string_of_int k ^ "__"

let rec transform_expr state =
  function
  | Int _ as expr -> expr
  | Ident ident as expr ->
    if Hashtbl.mem state.idents.local ident then
      expr
    else
      let index = add_with_index state.idents.escape ident in
      Call (Ident "[", [Call (Ident "[", [Ident "env"; Int index]); Int 0])

  | Call (Ident ident, args) when Hashtbl.mem state.idents.global ident ->
    Call (Ident ident, List.map (transform_expr state) args)

  | Call (func, args) ->
    let args = List.map (transform_expr state) args in
    let func = transform_expr state func in

    let ident = match func with
      | Ident _ -> func
      | _ ->
        (
          let ident = generate_ident state in
          Queue.add (Let (ident, func)) state.stmts.local;
          Ident ident
        ) in

    Call (Call (Ident "[", [ident; Int 0]), Call (Ident "[", [ident; Int 1]) :: args)

  | Func (args, stmts) ->
    (
      let parent_state = state in
      let child_state = {
        parent_state with
        idents = {
          global = parent_state.idents.global;
          local = List.map (fun arg -> (arg, ())) args |> List.to_seq |> Hashtbl.of_seq;
          escape = Hashtbl.create 8;
          heap = Hashtbl.create 8;
        };
      } in

      let stmts =
        stmts
        |> List.map
          (
            fun stmt ->
              Queue.add (transform_stmt child_state stmt) child_state.stmts.local;
              let stmts = Queue.create () in
              Queue.transfer child_state.stmts.local stmts;
              stmts |> Queue.to_seq |> List.of_seq
          )
        |> List.concat
        |> List.map
          (
            function
            | Let (ident, expr) when Hashtbl.mem child_state.idents.heap ident ->
              Let (ident, Array [expr])
            | stmt -> stmt
          ) in

      parent_state.k <- child_state.k;

      let env =
        Hashtbl.to_seq child_state.idents.escape
        |> Seq.map
          (
            fun (ident, child_index) ->
              if Hashtbl.mem parent_state.idents.local ident then
                (
                  Hashtbl.replace parent_state.idents.heap ident ();
                  (Ident ident, child_index)
                )
              else
                let parent_index = add_with_index parent_state.idents.escape ident in
                (Call (Ident "[", [Ident "env"; Int parent_index]), child_index)
          )
        |> List.of_seq
        |> List.sort (fun a b -> snd a - snd b)
        |> List.map fst in

      let ident = generate_ident parent_state in
      Queue.add (Let (ident, Func ("env" :: args, stmts))) parent_state.stmts.global;
      Array [Ident ident; Array env]
    )

  | Array exprs -> Array (List.map (transform_expr state) exprs)

and transform_stmt state =
  function
  | Let (ident, expr) ->
    (
      let expr = transform_expr state expr in
      if Hashtbl.mem state.idents.local ident then assert false;
      Hashtbl.add state.idents.local ident ();
      Let (ident, expr)
    )

  | Set (expr_to, expr_from) ->
    let expr_from = transform_expr state expr_from in
    Set (transform_expr state expr_to, expr_from)

  | Expr expr -> Expr (transform_expr state expr)
  | Ret (Some expr) -> Ret (Some (transform_expr state expr))
  | Ret None as stmt-> stmt

let run_process args =
  In_channel.input_all (Unix.open_process_args_in args.(0) args)

let write_to_channel out_channel string =
  output_string out_channel string;
  output_string out_channel "\n\n"

let run_script expr state =
  let path = "/tmp/closure_convert.js" in
  let out_channel = open_out path in

  Queue.iter (fun stmt -> write_to_channel out_channel (show_stmt 0 stmt)) state.stmts.global;
  Queue.iter (fun stmt -> write_to_channel out_channel (show_stmt 0 stmt)) state.stmts.local;
  write_to_channel out_channel (show_stmt 0 (Expr expr));

  close_out out_channel;

  let result = run_process [|"node"; path|] in

  print_string (run_process [|"cat"; path|]);
  print_endline result;

  result

let () =
  let state = {
    idents = {
      global = Hashtbl.create 8;
      local = Hashtbl.create 8;
      escape = Hashtbl.create 8;
      heap = Hashtbl.create 8;
    };
    stmts = {
      global = Queue.create ();
      local = Queue.create ();
    };
    k = 0;
  } in

  List.iter (fun ident -> Hashtbl.add state.idents.global ident ()) ["console.log"; "["; "+"];

  let expr =
    Call (Func ([], [
        Let ("x", Int 0);
        Let ("y", Int (-1));
        Let ("counter", Func ([], [
            Expr (Call (Ident "console.log", [Ident "y"]));
            Ret (Some (Func ([], [
                Set (Ident "x", Call (Ident "+", [Ident "x"; Int 1]));
                Ret (Some (Ident "x"));
              ])))]));
        Let ("c", Call (Ident "counter", []));
        Expr (Call (Ident "console.log", [Call (Ident "c", [])]));
        Expr (Call (Ident "console.log", [Call (Ident "c", [])]));
        Expr (Call (Ident "console.log", [Call (Ident "c", [])]));
        Expr (Call (Ident "console.log", [Call (Call (Ident "counter", []), [])]));
      ]), []) in

  let before = run_script expr state in
  let expr = transform_expr state expr in
  let after = run_script expr state in

  assert (before = after)
